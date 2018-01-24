000000* MIT License
      * Copyright (c) 2018 Christer Stig Åke Landstedt
      * 
      * Permission is hereby granted, free of charge, to any person obtaining a copy
      * of this software and associated documentation files (the "Software"), to deal
      * in the Software without restriction, including without limitation the rights
      * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      * copies of the Software, and to permit persons to whom the Software is
      * furnished to do so, subject to the following conditions:
      * 
      * The above copyright notice and this permission notice shall be included in all
      * copies or substantial portions of the Software.
      * 
      * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      * SOFTWARE.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob-todo.
       AUTHOR.  "Christer Stig Åke Landstedt".

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT TODODATAFILE ASSIGN TO "cob-todo.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS TODOKEY.
           SELECT DONEDATAFILE ASSIGN TO "cob-todo-done.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS DONEKEY.
       
       DATA DIVISION.
         FILE SECTION.
         FD TODODATAFILE
           RECORD CONTAINS 80 CHARACTERS.
         01 TODODATAFILEFD.
           05 TODOKEY PIC X(2).
           05 TODOTEXT PIC X(25).
         FD DONEDATAFILE
           RECORD CONTAINS 80 CHARACTERS.
         01 DONEDATAFILEFD.
           05 DONEKEY PIC X(12).
           05 DONETEXT PIC X(25).
         WORKING-STORAGE SECTION.
         01 WS-ENDOFFILE PIC 9 VALUE ZERO. 
         01 WS-TODODATAFILEFD.
           05 WS-TODOKEY PIC X(2).
           05 WS-TODOTEXT PIC X(25).
         01 WS-DONEDATAFILEFD.
           05 WS-DONEKEY PIC X(12).
           05 WS-DONETEXT PIC X(25).
         01 DATEANDTIME.
           05 CURRENTDATE.
             10 YY PIC 99.
             10 MM PIC 99.
             10 DD PIC 99.
           05 CURRENTTIME.
             10 TIMEHH PIC 99.
             10 TIMEMM PIC 99.
             10 TIMESS PIC 99.
             10 TIMEMS PIC 99.

         LOCAL-STORAGE SECTION.
         01 USER-SELECTION PIC 9 VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
       999-SELECTION0.
       MOVE 0 TO USER-SELECTION.
       DISPLAY "---------------".
       DISPLAY "Cobol To do 0.2".
       DISPLAY "---------------".
       PERFORM UNTIL USER-SELECTION>0
         DISPLAY "MENU"
         DISPLAY "------------------------"
         DISPLAY "    ENTER YOUR CHOICE"
         DISPLAY "    1 : To do list"
         DISPLAY "    2 : Information"
         DISPLAY "    3 : Exit application"
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 GO TO 999-SELECTION1
           WHEN 2 GO TO 999-SELECTION2
           WHEN 3 GO TO 999-SELECTION3
           WHEN OTHER GO TO 999-SELECTION0ERROR
         END-EVALUATE
       END-PERFORM.

       999-SELECTION0ERROR.

       DISPLAY " ".
       DISPLAY "!!!ERROR IN INPUT".
       GO TO 999-SELECTION0.
       
       999-SELECTION1.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "----------".
       DISPLAY "To do list".
       DISPLAY "----------".

       OPEN I-O TODODATAFILE.
       PERFORM UNTIL WS-ENDOFFILE = 1
         READ TODODATAFILE INTO WS-TODODATAFILEFD
           AT END MOVE 1 TO WS-ENDOFFILE
           NOT AT END
           DISPLAY TODOKEY " " TODOTEXT
         END-READ    
       END-PERFORM.
       CLOSE TODODATAFILE.
       MOVE 0 TO WS-ENDOFFILE.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY " "
         DISPLAY "----------------------------------------"
         DISPLAY "MENU"
         DISPLAY "----------------------------------------"
         DISPLAY "    ENTER YOUR CHOICE"
         DISPLAY "    1 : Add task"
         DISPLAY "    2 : Change task"
         DISPLAY "    3 : Remove task"
         DISPLAY "    4 : Move task to completed task list"
         DISPLAY "    5 : See completed task list"
         DISPLAY "    6 : Go to main menu"
         DISPLAY "    7 : Exit application"
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 GO TO 999-SELECTION1ADD
           WHEN 2 GO TO 999-SELECTION1CHANGE
           WHEN 3 GO TO 999-SELECTION1REMOVE
           WHEN 4 GO TO 999-SELECTION1MOVE
           WHEN 5 GO TO 999-SELECTION1DONE
           WHEN 6 GO TO 999-SELECTION0
           WHEN 7 GO TO 999-SELECTION3
           WHEN OTHER GO TO 999-SELECTION1ERROR
         END-EVALUATE
       END-PERFORM.

       999-SELECTION1ERROR.

       DISPLAY " ".
       DISPLAY "!!!ERROR IN INPUT".
       GO TO 999-SELECTION1.

       999-SELECTION1ADD.
       MOVE 0 TO USER-SELECTION.

       DISPLAY " ".
       DISPLAY "Enter task number:".
       ACCEPT WS-TODOKEY.
       IF WS-TODOKEY IS NUMERIC
         DISPLAY "Enter task (max 25 characters):"
         ACCEPT WS-TODOTEXT
         ELSE
           DISPLAY "!!!ERROR IN INPUT"
           GO TO 999-SELECTION1ADD
       END-IF.
       DISPLAY " ".

       OPEN I-O TODODATAFILE.
       MOVE WS-TODOKEY TO TODOKEY.
       MOVE WS-TODOTEXT TO TODOTEXT.
       WRITE TODODATAFILEFD
         INVALID KEY DISPLAY"!!!ERROR RECORD ALREADY EXIST!"
         NOT INVALID KEY DISPLAY "Task added"
       END-WRITE.

       CLOSE TODODATAFILE.

       GO TO 999-SELECTION1.

       999-SELECTION1CHANGE.
       MOVE 0 TO USER-SELECTION.

       DISPLAY " ".
       DISPLAY "Enter task number:".
       ACCEPT WS-TODOKEY.
       IF WS-TODOKEY IS NUMERIC
         DISPLAY "Enter task (max 25 characters):"
         ACCEPT WS-TODOTEXT
         ELSE
           DISPLAY "!!!ERROR IN INPUT"
           GO TO 999-SELECTION1ADD
       END-IF.
       DISPLAY " ".

       OPEN I-O TODODATAFILE.
       MOVE WS-TODOKEY TO TODOKEY.
       MOVE WS-TODOTEXT TO TODOTEXT.
       REWRITE TODODATAFILEFD
         INVALID KEY DISPLAY"!!!ERROR TASK DOSE NOT EXIST!"
         NOT INVALID KEY DISPLAY "Task changed"
       END-REWRITE.

       CLOSE TODODATAFILE.

       GO TO 999-SELECTION1.

       999-SELECTION1REMOVE.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "-------------------------"
       DISPLAY "Enter the number of the task to be removed".
       ACCEPT WS-TODOKEY.
       IF WS-TODOKEY IS NUMERIC
         MOVE WS-TODOKEY TO TODOKEY
         ELSE
           DISPLAY "!!!ERROR IN INPUT"
           GO TO 999-SELECTION1REMOVE
       END-IF.
       OPEN I-O TODODATAFILE.
       DELETE TODODATAFILE
         INVALID KEY DISPLAY "!!!ERROR TASK DOSE NOT EXIST!"
         NOT INVALID KEY DISPLAY "Task removed"
       END-DELETE.
       CLOSE TODODATAFILE.

       GO TO 999-SELECTION1.

       999-SELECTION1MOVE.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "-------------------------"
       DISPLAY "Enter the number of the task to be moved".
       ACCEPT TODOKEY.
       OPEN I-O TODODATAFILE.
       READ TODODATAFILE INTO WS-TODODATAFILEFD
         KEY IS TODOKEY
         INVALID KEY DISPLAY "!!!ERROR"
       END-READ.
       CLOSE TODODATAFILE.

       ACCEPT CURRENTTIME FROM TIME.
       ACCEPT CURRENTDATE FROM DATE.
       MOVE DATEANDTIME TO DONEKEY.
       MOVE WS-TODOTEXT TO DONETEXT.

       OPEN I-O DONEDATAFILE.
       WRITE DONEDATAFILEFD
         INVALID KEY DISPLAY"!!!ERROR RECORD ALREADY EXIST!"
         NOT INVALID KEY DISPLAY "Task moved"
       END-WRITE.
       CLOSE DONEDATAFILE.

       OPEN I-O TODODATAFILE.
       DELETE TODODATAFILE
         INVALID KEY DISPLAY "!!!ERROR TASK DOSE NOT EXIST!"
         NOT INVALID KEY DISPLAY "Task removed"
       END-DELETE.
       CLOSE TODODATAFILE.

       GO TO 999-SELECTION1.

       999-SELECTION1DONE.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "----------".
       DISPLAY "Done list".
       DISPLAY "----------".

       OPEN I-O DONEDATAFILE.
       PERFORM UNTIL WS-ENDOFFILE = 1
         READ DONEDATAFILE
           AT END MOVE 1 TO WS-ENDOFFILE
           NOT AT END
           DISPLAY DONEKEY " " DONETEXT
         END-READ    
       END-PERFORM.
       CLOSE DONEDATAFILE.
       MOVE 0 TO WS-ENDOFFILE.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY " "
         DISPLAY "----------------------------------------"
         DISPLAY "MENU"
         DISPLAY "----------------------------------------"
         DISPLAY "    ENTER YOUR CHOICE"
         DISPLAY "    1 : Remove done task"
         DISPLAY "    2 : Remove all done tasks"
         DISPLAY "    3 : Go to To do list"
         DISPLAY "    4 : Go to main menu"
         DISPLAY "    5 : Exit application"
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 GO TO 999-SELECTION1DONEREMOVE
           WHEN 2 GO TO 999-SELECTION1DONEREMOVEALL
           WHEN 3 GO TO 999-SELECTION1
           WHEN 4 GO TO 999-SELECTION0
           WHEN 5 GO TO 999-SELECTION3
           WHEN OTHER GO TO 999-SELECTION1ERROR
         END-EVALUATE
       END-PERFORM.

       999-SELECTION1DONEERROR.

       DISPLAY " ".
       DISPLAY "!!!ERROR IN INPUT".
       GO TO 999-SELECTION1DONE.

       999-SELECTION1DONEREMOVE.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "------------------------------------------"
       DISPLAY "Enter the number of the task to be removed".
       ACCEPT WS-DONEKEY.
       IF WS-TODOKEY IS NUMERIC
         MOVE WS-DONEKEY TO DONEKEY
         ELSE
           DISPLAY "!!!ERROR IN INPUT"
           GO TO 999-SELECTION1DONEREMOVE
       END-IF.
       OPEN I-O DONEDATAFILE.
       DELETE DONEDATAFILE RECORD
         INVALID KEY DISPLAY "!!!ERROR TASK DOSE NOT EXIST!"
         NOT INVALID KEY DISPLAY "Task removed"
       END-DELETE.
       CLOSE DONEDATAFILE.

       GO TO 999-SELECTION1DONE.

       999-SELECTION1DONEREMOVEALL.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "----------------------------------------".

       DELETE FILE
         DONEDATAFILE
       END-DELETE.

       OPEN OUTPUT DONEDATAFILE.
       MOVE " " TO DONEKEY.
       MOVE " " TO DONETEXT.
       WRITE DONEDATAFILEFD
       END-WRITE.
       CLOSE DONEDATAFILE.

       GO TO 999-SELECTION1DONE.

       999-SELECTION2.
       MOVE 0 TO USER-SELECTION.

       DISPLAY " ".
       DISPLAY "-----------------------".
       DISPLAY "Application information".
       DISPLAY "-----------------------".
       DISPLAY "Application: Cobol To do 0.2".
       DISPLAY "Made with: Ubuntu 16.04 and GnuCobol(OpenCobol) 2.2".
       DISPLAY "---------------------------------------------------".
       DISPLAY "MIT License".
       DISPLAY "Copyright (c) 2018 Christer Stig Åke Landstedt".
       DISPLAY " ".
       DISPLAY 
        "Permission is hereby granted, free of charge, to any "
        "person obtaining a copy of this software and "
        "associated documentation files (the ""Software""), "
        "to deal in the Software without restriction, "
        "including without limitation the rights "
        "to use, copy, modify, merge, publish, distribute, "
        "sublicense, and/or sell copies of the Software,"
        "and to permit persons to whom the Software is "
        "furnished to do so, subject to the following "
        "conditions:".
       DISPLAY " ".
       DISPLAY 
        "The above copyright notice and this permission notice "
        "shall be included in all copies or substantial "
        "portions of the Software.".
       DISPLAY " ".
       DISPLAY 
        "THE SOFTWARE IS PROVIDED ""AS IS"", WITHOUT WARRANTY "
        "OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT "
        "LIMITED TO THE WARRANTIES OF MERCHANTABILITY, "
        "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. "
        "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS "
        "BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER "
        "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR "
        "OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION "
        "WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE "
        "SOFTWARE.".
       GO TO 999-SELECTION0.
       

       999-SELECTION3.
       MOVE 0 TO USER-SELECTION.
       STOP-RUN.
