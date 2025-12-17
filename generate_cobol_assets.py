import os

def generate_cobol_assets():
    base_dir = r"C:\Users\Hp\Antigravity\cobol_sample"
    dirs = {
        "copybooks": os.path.join(base_dir, "COPYBOOKS"),
        "sources": os.path.join(base_dir, "SOURCES"),
        "jcl": os.path.join(base_dir, "JCL")
    }

    for path in dirs.values():
        os.makedirs(path, exist_ok=True)

    print(f"Created directories in {base_dir}")

    for i in range(1, 31):
        suffix = f"{i:02d}"
        
        # 1. Copybook
        cpy_name = f"REC{suffix}"
        cpy_content = f"""      ******************************************************************
      * COPYBOOK: {cpy_name}
      * DESCRIPTION: SAMPLE RECORD STRUCTURE FOR MODULE {suffix}
      ******************************************************************
       01  EMP-RECORD-{suffix}.
           05  EMP-ID-{suffix}          PIC 9(05).
           05  EMP-NAME-{suffix}        PIC X(20).
           05  EMP-DEPT-{suffix}        PIC X(10).
           05  EMP-SALARY-{suffix}      PIC 9(07)V99.
           05  EMP-STATUS-{suffix}      PIC X(01).
"""
        with open(os.path.join(dirs["copybooks"], f"{cpy_name}.cpy"), "w") as f:
            f.write(cpy_content)

        # 2. Subroutine
        sub_name = f"SUBR{suffix}"
        sub_content = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {sub_name}.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY {cpy_name}.
       PROCEDURE DIVISION USING EMP-RECORD-{suffix}.
       MAIN-LOGIC.
           MOVE 'FULL TIME' TO EMP-STATUS-{suffix}
           ADD 1000 TO EMP-SALARY-{suffix}
           DISPLAY 'SUBROUTINE {sub_name} EXECUTED FOR ID: ' EMP-ID-{suffix}
           GOBACK.
"""
        with open(os.path.join(dirs["sources"], f"{sub_name}.cbl"), "w") as f:
            f.write(sub_content)

        # 3. Main Program
        main_name = f"MAIN{suffix}"
        main_content = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. {main_name}.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY {cpy_name}.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 12345 TO EMP-ID-{suffix}
           MOVE 'JOHN DOE' TO EMP-NAME-{suffix}
           MOVE 'IT' TO EMP-DEPT-{suffix}
           MOVE 50000.00 TO EMP-SALARY-{suffix}
           
           DISPLAY 'CALLING SUBROUTINE {sub_name}...'
           CALL '{sub_name}' USING EMP-RECORD-{suffix}
           
           DISPLAY 'RETURNED STATUS: ' EMP-STATUS-{suffix}
           DISPLAY 'UPDATED SALARY: ' EMP-SALARY-{suffix}
           STOP RUN.
"""
        with open(os.path.join(dirs["sources"], f"{main_name}.cbl"), "w") as f:
            f.write(main_content)

        # 4. JCL
        jcl_name = f"JOB{suffix}"
        jcl_content = f"""//JOB{suffix} JOB (ACCT),'COBOL JOB',CLASS=A,MSGCLASS=X
//*
//* COMPILE AND RUN FOR SET {suffix}
//*
//STEP1    EXEC IGYWCL,PARM.COBOL='LIB'
//COBOL.SYSIN DD DSN=USER.COBOL.SOURCE({main_name}),DISP=SHR
//COBOL.SYSLIB DD DSN=USER.COBOL.COPYBOOKS,DISP=SHR
//*
//STEP2    EXEC PGM={main_name}
//STEPLIB  DD DSN=USER.COBOL.LOAD,DISP=SHR
//SYSOUT   DD SYSOUT=*
"""
        with open(os.path.join(dirs["jcl"], f"{jcl_name}.jcl"), "w") as f:
            f.write(jcl_content)

    print("Generation complete.")

if __name__ == "__main__":
    generate_cobol_assets()
