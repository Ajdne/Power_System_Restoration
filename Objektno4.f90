program Objektno4

    !Use Power_Restoration
    Use Recursion
    Use Printer
    Implicit none

    Call Load_Grid_Data
        Print *, "Grid data loaded!"
        Print *, "___________________________________________________________"
        Write (*, *) "Number of nodes:", Number_of_Nodes
        Write(*, *) "Number of branches:", Number_of_Branches
        Print *, "Short circuit location:", Short_Circuit_Location
        Print *, "-----------------------------------------------------------"
        Read (*, *)
    
        Call Print_Table    ! Uncomment this part to check table data
        Read (*, *)    

    Call Find_Short_Circuit
        Print *, "___________________________________________________________"
        Print *, "Short circuit found and B zone defined!"
        Print *, "-----------------------------------------------------------"
        Read (*, *)

        Call Print_Table    ! Uncomment this part to check table data
        Read (*, *) 

    Call Find_Branching
        Print *, "___________________________________________________________"
        Print *, "Branching nodes found!"
        Print *, "-----------------------------------------------------------"
        Read (*, *)

        Call Print_Table    ! Uncomment this part to check table data
        Read (*, *) 

    Call Recursive_Turn_Off     ! All elements connected to broken branch are disconnected
        Print *, "___________________________________________________________"
        Print *, "Short circuit canceled and affected elements disconnected!"
        Print *, "-----------------------------------------------------------"
        Print *, "A and C zones defined!"
        Print *, "-----------------------------------------------------------"
        Read (*, *)

        Call Print_Table    ! Uncomment this part to check table data
        Read (*, *)  

    Call Print_Zone_A       ! This subroutine call the subroutine for restoring power to elements in zone A
        Read (*, *)

        Open (2, File = 'Result_Output.txt')

    Call Print_Zone_C       ! This subroutine calls another subroutine (C_Zone_Potentials)
                            ! which check for different ways to power up zone C
        Print *, "___________________________________________________________"
        Print *, "Results are shown in Result_Output.txt!"
        Print *, "-----------------------------------------------------------"
        Read (*, *)

        Close (2)       ! Closing the output file

    Deallocate (Node_Type)
    Deallocate (Branch_Type)

End program Objektno4