/* 
 * msh - A mini shell program with job control
 * 
 * <Put your name and login ID here>
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include "util.h"
#include "jobs.h"


/* Global variables */
int verbose = 0;            /* if true, print additional output */

extern char **environ;      /* defined in libc */
static char prompt[] = "msh> ";    /* command line prompt (DO NOT CHANGE) */
static struct job_t jobs[MAXJOBS]; /* The job list */

/* End global variables */


/* Function prototypes */

/* Here are the functions that you will implement */
void eval(char *cmdline);
int builtin_cmd(char **argv);
void do_bgfg(char **argv);
void waitfg(pid_t pid);

void sigchld_handler(int sig);
void sigtstp_handler(int sig);
void sigint_handler(int sig);

/* Here are helper routines that we've provided for you */
void usage(void);
void sigquit_handler(int sig);



/*
 * main - The shell's main routine 
 */
int main(int argc, char **argv) 
{
    char c;
    char cmdline[MAXLINE];
    int emit_prompt = 1; /* emit prompt (default) */

    /* Redirect stderr to stdout (so that driver will get all output
     * on the pipe connected to stdout) */
    dup2(1, 2);

    /* Parse the command line */
    while ((c = getopt(argc, argv, "hvp")) != EOF) {
        switch (c) {
            case 'h':             /* print help message */
                usage();
            break;
            case 'v':             /* emit additional diagnostic info */
                verbose = 1;
            break;
            case 'p':             /* don't print a prompt */
                emit_prompt = 0;  /* handy for automatic testing */
            break;
            default:
                usage();
        }
    }

    /* Install the signal handlers */

    /* These are the ones you will need to implement */
    Signal(SIGINT,  sigint_handler);   /* ctrl-c */
    Signal(SIGTSTP, sigtstp_handler);  /* ctrl-z */
    Signal(SIGCHLD, sigchld_handler);  /* Terminated or stopped child */

    /* This one provides a clean way to kill the shell */
    Signal(SIGQUIT, sigquit_handler); 

    /* Initialize the job list */
    initjobs(jobs);

    /* Execute the shell's read/eval loop */
    while (1) {

        /* Read command line */
        if (emit_prompt) {
            printf("%s", prompt);
            fflush(stdout);
        }
        if ((fgets(cmdline, MAXLINE, stdin) == NULL) && ferror(stdin))
            app_error("fgets error");
        if (feof(stdin)) { /* End of file (ctrl-d) */
            fflush(stdout);
            exit(0);
        }

        /* Evaluate the command line */
        eval(cmdline);
        fflush(stdout);
        fflush(stdout);
    } 

    exit(0); /* control never reaches here */
}
  
/* 
 * eval - Evaluate the command line that the user has just typed in
 * 
 * If the user has requested a built-in command (quit, jobs, bg or fg)
 * then execute it immediately. Otherwise, fork a child process and
 * run the job in the context of the child. If the job is running in
 * the foreground, wait for it to terminate and then return.  Note:
 * each child process must have a unique process group ID so that our
 * background children don't receive SIGINT (SIGTSTP) from the kernel
 * when we type ctrl-c (ctrl-z) at the keyboard.  
*/
void eval(char *cmdline) {

    //Taken from book page 735
    //pointer usage taken from Piazza @185
    char *argv[MAXARGS]; /* Argument list execve() */
    char buf[MAXLINE]; /* Holds modified command line */
    int bg; /* should the job run in bg or fg? */
    pid_t pid = 0;      /* Process id */
    sigset_t mask;
    signal(SIGCHLD, sigchld_handler);

    strcpy(buf, cmdline);
    bg = parseline(buf, argv);
    if (argv[0] == NULL)
        return; /* Ignore empty lines */

    int ret = builtin_cmd(argv);
    printf("Is it built in? %d\n",ret);
    printf("Are we running in background? %d\n",bg);

    //block child so that addjob and deletejob will be called in the right order
    sigemptyset(&mask);
    sigaddset(&mask, SIGCHLD); 
    sigprocmask(SIG_BLOCK, &mask, NULL); //from book 757

    if (ret==0) {//is not a builtin command

        if ((pid = fork()) == 0) {  /* Child runs user job in background */
                setpgid(0, 0); //work around for sigint
 

            sigprocmask(SIG_UNBLOCK, &mask, NULL); /* unblock sigchild */
            //signal(SIGINT, sigint_handler);
            
            if (execve(argv[0], argv, environ) < 0) {
                printf("%s: Command not found. \n", argv[0]);
                                //waitfg(pid);
                                //kill(pid, SIGKILL);
                exit(0);
            }
            else{
                 printf("command WAS found \n");
                 exit(0);
            }
            
                return;
        }   

        /* Parent waits for foreground job to terminate */
        //doesnt the IF statement mean that we are waiting on the BACKGROUND job?
        //should we wait on a background job?

        //wait until all foreground jobs are over and then kill children and quit
        if (bg==1) {

               addjob(jobs,pid,FG,cmdline);
               sigprocmask(SIG_UNBLOCK, &mask, NULL);
               waitfg(pid);
               //signal(SIGINT, sigint_handler);
               // signal(SIGCONT, sigint_handler);
        } else {
            printf("%d %s", pid, cmdline);
            addjob(jobs, pid, BG, cmdline);
            sigprocmask(SIG_UNBLOCK, &mask, NULL);
        }
    }else{
        execve(argv[0], argv, environ);
    }  
       
    return;
}



/* 
 * builtin_cmd - If the user has typed a built-in command then execute
 *    it immediately.  
 * Return 1 if a builtin command was executed; return 0
 * if the argument passed in is *not* a builtin command.
 */
int builtin_cmd(char **argv) 
{
//Taken from book page 735
    if (!strcmp(argv[0], "quit")) /* quit command */
        exit(0);
        else if (!strcmp(argv[0], "&"))    /* Ignore singleton & */
            return 1;
        else if (!strcmp(argv[0], "fg")){
            do_bgfg(argv);
            return 1;
        } else if (!strcmp(argv[0], "bg")){
            do_bgfg(argv);
        return 1;
    } else if (!strcmp(argv[0], "jobs")){
        listjobs(jobs);
        return 1;
    }
    return 0;     /* not a builtin command */
}

/* 
 * do_bgfg - Execute the builtin bg and fg commands
 */
void do_bgfg(char **argv) 
{
struct job_t *job; //initilize job structure
    int j; // variable that holds jib or pid
// pointer ideas were taken from piazza @185

/*char i = argv[1][0];
int j = argv[1][1];
for (i=0; i < 4; i++)
printf("argv[%d] = %s\n", i, argv[i]);
for (i=0; i < 3; i++) {
  for (j = 0; j < 2; j++ ) {
      printf("argv[%d][%d] = %s\n", i, j, argv[i][j]);
   }
}
      //printf("argv[1][0] = %s\n", i);
      printf("argv[1][1] = %d\n", j);  */



    if(strcmp(argv[0], "fg") == 0) // using string compare fuction; 0 returns if equal
    {  
               if(argv[1] == NULL)
                {
                        printf(" missing arguements\n");
                        return;
                }
        else if(argv[1][0] == '%') // % has been passed indicating job id
        {
            j = (&argv[1][1]); /*setting job id; haven't done research yet in why, but from testing
                                               number entered is always off by 48 */

            job = getjobjid(jobs, j);  //getting job ID
 
            if(job == NULL) //if null job doesn't exist
                printf("%d: job is missing\n", j);
        }
        else{
            j = atoi(argv[1]); //PID was passed

            job = getjobpid(jobs, j);    //get the job pid

            if(job == NULL) //if null process doesn't exist
                printf("(%d): process is missing \n",j);
        }

        if(job != NULL)  
        {
                        //pid_t pid = getjobpid(jobs, j);  // get job pid and set it to pid 

            kill(job->pid, SIGCONT);    // send continue signal; taken from Piazza @185

                        job->state = FG;
                        
                         printf("going into waitfg ---- \n");
            waitfg(job->pid); //call waitfg function; taken from Piazza @185

            if(job->state != ST)   //if job is not stopped; taken from Piazza @185

              deletejob(jobs, job->pid);  //remove job from the list

        
        }   

    }

    if(strcmp(argv[0], "bg") == 0) // using string compare function to check if it is bg; if it is it will return 0
        {   
        if(argv[1] == NULL)
                {  
            printf(" missing arguements\n");
            return;
                }
        else if(argv[1][0] == '%') // % has been passed indicating job id
        { 
            j = (&(argv[1][1])); /*setting job id; haven't done research yet in why, but from testing 
                                               number entered is always off by 48 */  

            job = getjobjid(jobs, j); // getting job ID 

            if(job == NULL) //if null job doesn't exist
                printf("%d: job is missing \n", j);
        }
        else
        {
            j = atoi(argv[1]); //PID was passed

            job = getjobpid(jobs, j);  //get job pid 

            if(job == NULL) //if null process doesn't exit
                printf("(%d): process is missing \n",j);
        }
               
    
        printf("[%d] (%d) %s\n",job->jid, job->pid, job->cmdline); //out put job info that is being backgrouded

        kill(job->pid,SIGCONT);  // continuing the process in the back group.

        job->state=BG; //changing from stopped to back ground

    }
    return;
}

/* 
 * waitfg - Block until process pid is no longer the foreground process
 */
void waitfg(pid_t pid){

    struct job_t *job; //set up job structure
    job = getjobpid(jobs, pid); //get job pid

printf("JOB::::::::: %d\n", job);
printf("FGJOB::::::: %d\n", fgpid(jobs));
    if (job == NULL){ //job is no longer running
        
        job->state = ST; // set state to stopped

        kill(pid, SIGKILL); //kill job 
        deletejob(jobs, pid); //delete from job list

        return;
    }
printf("JOB::::::::: %d\n", job);
printf("FGJOB::::::: %d\n", fgpid(jobs));
    while (fgpid(jobs) == pid) { //looping until FG state is gone
                printf("---job state %d-\n", job->state);
        sleep(1); //sleep for 1 second
    }

//printf("hit-1-----------------\n");
return;
}

/*****************
 * Signal handlers
 *****************/

/* 
 * sigchld_handler - The kernel sends a SIGCHLD to the shell whenever
 *     a child job terminates (becomes a zombie), or stops because it
 *     received a SIGSTOP or SIGTSTP signal. The handler reaps all
 *     available zombie children, but doesn't wait for any other
 *     currently running children to terminate.  
 */
void sigchld_handler(int sig) 
{
int status; 

/*     below while loop was provided in the waitpid man pages. */
pid_t w;

do {
                   w = waitpid(-1, &status, WUNTRACED | WCONTINUED);
                   if (w == -1) {
                       unix_error("waitpid");
                       exit(EXIT_FAILURE);
                   }

                   if (WIFEXITED(status)) {
                       deletejob(jobs, w);
                       //printf("exited, status=%d\n", WEXITSTATUS(status));
                   } else if (WIFSIGNALED(status)) {
                       printf("Job [%d] (%d) terminated by signal 2\n", pid2jid(jobs, w),w); // display interupt message
                   } else if (WIFSTOPPED(status)) {
                       printf("Job [1] (%d) stopped by signal 2\n", w); //print stop message
                   } else if (WIFCONTINUED(status)) {
                       printf("continued\n");
                   }
               } while (!WIFEXITED(status) && !WIFSIGNALED(status));
               //exit(EXIT_SUCCESS);


//printf("hit--2222------------\n");
return;
}

/* 
 * sigint_handler - The kernel sends a SIGINT to the shell whenver the
 *    user types ctrl-c at the keyboard.  Catch it and send it along
 *    to the foreground job.  
 */
void sigint_handler(int sig) {

    struct job_t *job; //job structure
    pid_t pid;
    pid = fgpid(jobs); // get fg pid
    job = getjobpid(jobs, pid);
    printf("pid--->  %d\n",pid );

     if (pid > 0){  // if pid is not = to 0, which is a child running in background
     
        //printf("Job [1] (%d) terminated by signal 2\n", pid); // display interupt message

        // deletejob(jobs,pid);  //remove the job from the job list
        printf("1. Job state in sigint --- %d\n", job->state);
        job->state = UNDEF;
        printf("2. Job state in sigint --- %d\n", job->state);
        kill(pid, SIGINT); // send interupt signal

    }else{
            printf("CHILD CTRL C ---  \n" );
            printf("IS only run for foreground jobs. This will be ignored since no foreground job present ---  \n" );
            printf("Question - what if there are no jobs running, will the CTRL+C also return a pid of 0?  ---  \n" );
    }

    return;
}

/*
 * sigtstp_handler - The kernel sends a SIGTSTP to the shell whenever
 *     the user types ctrl-z at the keyboard. Catch it and suspend the
 *     foreground job by sending it a SIGTSTP.  
 */
void sigtstp_handler(int sig) 
{
struct job_t *job;//initialing job struct
pid_t pid; //group pid
pid = fgpid(jobs); // get fg pid

     if (pid > 0) // if pid is not = to 0
     {
         //printf("Job [1] (%d) stopped by signal 2\n", pid); //print stop message

         job->state=ST; //changing state to Stopped; taken from Piazza @185
         kill(pid, SIGTSTP); // send stop signal

     }


    return;
}

/*********************
 * End signal handlers
 *********************/



/***********************
 * Other helper routines
 ***********************/

/*
 * usage - print a help message
 */
void usage(void) 
{
    printf("Usage: shell [-hvp]\n");
    printf("   -h   print this message\n");
    printf("   -v   print additional diagnostic information\n");
    printf("   -p   do not emit a command prompt\n");
    exit(1);
}

/*
 * sigquit_handler - The driver program can gracefully terminate the
 *    child shell by sending it a SIGQUIT signal.
 */
void sigquit_handler(int sig) 
{
    ssize_t bytes;
    const int STDOUT = 1;
    bytes = write(STDOUT, "Terminating after receipt of SIGQUIT signal\n", 45);
    if(bytes != 45)
       exit(-999);
    exit(1);
}



