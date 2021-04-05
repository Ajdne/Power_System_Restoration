Module Printer
    
    Use Power_Restoration
    Use Recursion
    Implicit None

    Integer, Private :: Count

    Contains ! ========================================================================

        Subroutine Print_Zone_A
            Implicit None

            Count = 0

            Do j = 1, Number_of_Nodes
                If ( Node_Type(j)%Get_Zone() == 'A' ) Then
                    Count = Count + 1
                    Go to 555
                End If
            End Do

            Do i = 1, Number_of_Branches
                If ( Branch_Type(i)%Get_Zone() == 'A' ) Then
                    Count = Count + 1
                    Exit
                End If
            End Do

            555 If ( Count == 0 ) Then
                Print *, "___________________________________________________________"
                Print *, "No elements in zone A!"
                Print *, "--------------------------------------------"
            Else
                Call Restoring_Zone_A   ! Restoring power to elements in zone A

                Print *, "___________________________________________________________"
                Print *, "Power restored to zone A!"
                Print *, "--------------------------------------------"
            End If

        End Subroutine
    ! ---------------------------------------------------------------------------------
        Subroutine Print_Zone_C
            Implicit None

            Count = 0

            Do j = 1, Number_of_Nodes
                If ( Node_Type(j)%Get_Zone() == 'C' ) Then
                    Count = Count + 1
                    Go To 666
                End If
            End Do

            Do i = 1, Number_of_Branches
                If ( Branch_Type(i)%Get_Zone() == 'C' ) Then
                    Count = Count + 1
                    Exit
                End If
            End Do

            666 If ( Count == 0 ) Then
                Print *, "___________________________________________________________"
                Print *, "No elements in zone C!"
                Print *, "-----------------------------------------------------------" 
                Read (*, *)
                Call Print_Output

            Else    ! No need to call the following subroutines if there are no elements in zone C
                Call Find_Reserve_Branches    
                
                Print *, "___________________________________________________________"
                Print *, "Potential reserve branches found!"
                Print *, "-----------------------------------------------------------"
                Read (*, *)

                Call C_Zone_Potentials  
                Print *, "___________________________________________________________"
                Print *, "Ways to power zone C found!"
                Print *, "-----------------------------------------------------------"
                Read (*, *)

!                Call Print_Table
!                Read (*, *)
                Call Variations_of_C_Zone_Restoration

            End If

        End Subroutine Print_Zone_C
    ! ---------------------------------------------------------------------------------
        Subroutine Variations_of_C_Zone_Restoration
            Implicit None

            Do k = 1, X_Counter

!                Print *, "Connecting zone C, variation:", k

                Do i = 1, Number_of_Branches
                    If ( Branch_Type(i)%Get_Zone() == 'X' ) Then

                        Call Zone_C_Reconnect ( Branch_Type(i)%Get_ID() )   ! Enters the recursive subroutine

                        Write (2, *) '_____________________________________________________'
                        Write (2, *) "Variation", k
                        Write (2, *) "________"
                        Write (2, *) "| 5000 |"
                        Write (2, *) "^^^^^^^^"
                        Call Print_Output      ! This needs to be called after every variation

                        Exit
                    End if
                End Do
                
!                Call Print_Table
!                Read (*, *)

!                Print *, "Disconnecting zone C, after variation:", k

                Do i = 1, Number_of_Branches
                    If ( Branch_Type(i)%Get_Zone() == 'Y' ) Then
                        Call Zone_C_Disconnect ( Branch_Type(i)%Get_ID() )
                        Exit
                    End If
                End Do

!                Call Print_Table
!                Read (*, *)

            End Do            

        End Subroutine Variations_of_C_Zone_Restoration
    ! ------------------------------------------------------------------------------------------
        Subroutine  Print_Output
            Implicit None
                
                Do m = 1, Number_of_Branches
                    If ( Branch_Type(m)%Get_Start() == 5000 ) Then

                        If ( ( Branch_Type(m)%Get_Status() == .true. ) .AND. ( Branch_Type(m)%Get_Print() == .false. ) ) Then  
                            ! If the branch starts in root node and has not yet been printed in output file
                            Write (2, "(4x, '|')", Advance = 'No')
                            Call Recursive_Print_Branch ( Branch_Type(m)%Get_ID() )
                            
                        Else 
                            Write (2, "(4x, '|---')", Advance = 'No')
                        End If

                        Do n = 1, Number_of_Nodes
                            If ( ( Node_Type(n)%Get_Branching() == .true. ) .AND. ( Node_Type(n)%Get_Print() == .true. ) ) Then

                                Call Branching_Print ( Node_Type(n)%Get_ID() )

                            End If
                        End Do

                        Write (2, "(/, 4x, '|', /, 4x, '|')")   ! / is foe 'next line'

                    End If
                End Do
                
                Do i = 1, Number_of_Branches
                    Call Branch_Type(i)%Set_Print(.false.)  ! Need to 'reset' Print field in order for this subroutine to work
                End Do

                Do j = 1, Number_of_Nodes
                    Call Node_Type(j)%Set_Print(.false.)
                End Do

        End Subroutine Print_Output
!-----------------------------------------------------------------------------------------------

!____________ Subroutines for error checking and printing out on terminal ______________________

        Subroutine Print_Branch  (x) ! Subroutine for testing and checking for errors
            Implicit None
                
                Integer :: x

                Print *, Branch_Type(x)%Get_ID(), Branch_Type(x)%Get_Status(), Branch_Type(x)%Get_Zone(), Branch_Type(x)%Get_Start(), Branch_Type(x)%Get_End(), Branch_Type(x)%Get_Reserve()

        End Subroutine Print_Branch
!-----------------------------------------------------------------------------------------------
        Subroutine Print_Node  (x) ! Subroutine for testing and checking for errors
            Implicit None
                
                Integer :: x

                Print *, Node_Type(x)%Get_ID(), Node_Type(x)%Get_Status(), Node_Type(x)%Get_Zone(), Node_Type(x)%Get_Branching(), Node_Type(x)%Get_Source()

        End Subroutine Print_Node
!-----------------------------------------------------------------------------------------------
        Subroutine Print_Table  ! Subroutine for testing and checking for errors
            Implicit None
                
                Print *, "============================================"
                Print *, "NODES_____________"
                Print *, "ID/Status/Zone/Branching/Source" 

                Do i = 1, Number_of_Nodes
                    Call Print_Node(i)
                End Do

                Print *, "--------------------------------------------"
                Print *, "BRANCHES_____________"
                Print *, "ID/Status/Zone/Start/End/Reserve"

                Do i = 1, Number_of_Branches
                    Call Print_Branch(i)    
                End Do

                Print *, "============================================"

        End Subroutine Print_Table
!-----------------------------------------------------------------------------------------------
End Module Printer