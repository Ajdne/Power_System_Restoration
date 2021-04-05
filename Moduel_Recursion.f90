Module Recursion

    Use Power_Restoration
    Implicit None

    Contains ! ========================================================

        Subroutine Recursive_Turn_Off   ! Turns off and disconnect all elements connected to the broken branch
            Implicit None

            Call Shutdown_Upstream(Short_Circuit_Location)      ! Calling subroutine that disconnect all elements "above"
            Call Shutdown_Downstream(Short_Circuit_Location)    ! and "below" the broken element

        End Subroutine Recursive_Turn_Off
    ! -------------------------------------------------------------------------
        Recursive Subroutine Shutdown_Upstream (Starting_Element)   
            Implicit None

            Integer, Intent(In) :: Starting_Element

            Do i = 1, Number_of_Branches        ! If Starting_Element is a branch
                If ( Starting_Element == Branch_Type(i)%Get_ID() ) Then
                    Do j = 1, Number_of_Nodes

                        If ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) Then
                            Call Node_Type(j)%Set_Status(.false.)
                            Call Node_Type(j)%Set_Zone('A')
                            Call Shutdown_Upstream( Node_Type(j)%Get_ID() )   ! Then enter recursive subroutine with a node
                        End If

                    End Do
                End If
            End Do
            
            Do j = 1, Number_of_Nodes           ! If Starting_Element is a node
                If ( Starting_Element == Node_Type(j)%Get_ID() ) Then
                    Do i = 1, Number_of_Branches

                        If ( Node_Type(j)%Get_Source() == Branch_Type(i)%Get_ID() ) Then
                            Call Branch_Type(i)%Set_Status(.false.)
                            Call Branch_Type(i)%Set_Zone('A')
                            Call Shutdown_Upstream( Branch_Type(i)%Get_ID() ) ! Then enter recursive subroutine with a branch
                        End If

                    End Do
                End If
            End Do
            ! _____________ Now to check for branching ... 
            Do j = 1, Number_of_Nodes   
                If ( ( Node_Type(j)%Get_Branching() == .true. ) .AND. (Node_Type(j)%Get_Status() == .false. ) ) Then
                    Do i = 1, Number_of_Branches

                        If ( ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) .AND. ( Branch_Type(i)%Get_Status() /= .false. ) ) Then
                            Call Branch_Type(i)%Set_Status(.false.)
                            Call Branch_Type(i)%Set_Zone('A')
                            Call Shutdown_Downstream( Branch_Type(i)%Get_ID() )
                        End If

                    End Do
                End If
            End Do

        End Subroutine Shutdown_Upstream
    ! -------------------------------------------------------------------------
        Recursive Subroutine Shutdown_Downstream (Starting_Element)
            Implicit None

            Integer, Intent(In) :: Starting_Element

            Do i = 1, Number_of_Branches        ! If Starting_Element is a branch
                If ( Starting_Element == Branch_Type(i)%Get_ID() ) Then
                    Do j = 1, Number_of_Nodes

                        If ( Branch_Type(i)%Get_ID() == Node_Type(j)%Get_Source() ) Then
                            Call Node_Type(j)%Set_Status(.false.)

                            If ( Branch_Type(i)%Get_Zone() == 'A' ) Then    ! If the branch belongs to the A zone
                                Call Node_Type(j)%Set_Zone('A')             ! then so does the node it powers
                            Else If ( Branch_Type(i)%Get_Zone() == 'B' ) Then
                                Call Node_Type(j)%Set_Source(0)     ! Disconnecting node from broken source branch
                                Call Node_Type(j)%Set_Zone('C')     ! If the source branch is not zone A, then it is either
                            Else                                    ! zone B or C, which means node should be zone C
                                Call Node_Type(j)%Set_Zone('C')
                            End If                                  

                            Call Shutdown_Downstream( Node_Type(j)%Get_ID() )   ! Then enter recursive subroutine with a node

                        End If
                    End Do
                End If
            End Do
            
            Do j = 1, Number_of_Nodes           ! If Starting_Element is a node
                If ( Starting_Element == Node_Type(j)%Get_ID() ) Then
                    Do i = 1, Number_of_Branches

                        If ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_Start() ) Then
                            Call Branch_Type(i)%Set_Status(.false.)

                            If ( Node_Type(j)%Get_Zone() == 'A' ) Then 
                                Call branch_Type(i)%Set_Zone('A')             
                            Else 
                                Call Branch_Type(i)%Set_Zone('C')
                            End If

                            Call Shutdown_Downstream( Branch_Type(i)%Get_ID() ) ! Then enter recursive subroutine with a branch
                        End If

                    End Do
                End If
            End Do
            ! _____________ Now to check for branching ... 
            Do j = 1, Number_of_Nodes   
                If ( ( Node_Type(j)%Get_Branching() == .true. ) .AND. (Node_Type(j)%Get_Status() == .false. ) ) Then
                    Do i = 1, Number_of_Branches

                        If ( ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) .AND. ( Branch_Type(i)%Get_Status() /= .false. ) ) Then
                            Call Branch_Type(i)%Set_Status(.false.)

                            If ( Node_Type(j)%Get_Zone() == 'A' ) Then 
                                Call branch_Type(i)%Set_Zone('A')             
                            Else 
                                Call Branch_Type(i)%Set_Zone('C')
                            End If
           
                            Call Shutdown_Downstream( Branch_Type(i)%Get_ID() )
                        End If

                    End Do
                End If
            End Do

        End Subroutine Shutdown_Downstream
    ! -------------------------------------------------------------------------
        Recursive Subroutine Zone_C_Reconnect (Starting_Element_C)    ! Connecting C zone to the grid and restoring power to it
            Implicit None

            Integer, Intent(In) :: Starting_Element_C

!            Print *, 'Reconnect rekurzija!', Starting_Element_C

            Do i = 1, Number_of_Branches        ! If Starting_Element is a branch
                If ( Starting_Element_C == Branch_Type(i)%Get_ID() ) Then
                    
                    Do j = 1, Number_of_Nodes
                        If ( ( Node_Type(j)%Get_Zone() == 'C' ) .AND. ( Node_Type(j)%Get_Status() == .false. ) ) Then      ! This subroutine is only for nodes in zone C

                            If ( ( Branch_Type(i)%Get_End() == Node_Type(j)%Get_ID() ) .OR. ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) ) Then
                                If ( Branch_Type(i)%Get_ID() /= Node_Type(j)%Get_Source() ) Then
                                  
                                    Call Node_Type(j)%Set_Source( Branch_Type(i)%Get_ID() )

                                    If ( Branch_Type(i)%Get_Zone() == 'X' ) Then
                                        Call Branch_Type(i)%Set_Zone('Y')
                                    End If

                                End If

                                Call Branch_Type(i)%Set_Status(.true.)
                                Call Node_Type(j)%Set_Status(.true.)

                                Call Zone_C_Reconnect ( Node_Type(j)%Get_ID() ) ! Then enter recursive subroutine with a node
                            End If

                        End If
                    End Do

                End If
            End Do

            Do j = 1, Number_of_Nodes           ! If Starting_Element is a node
                If ( Starting_Element_C == Node_Type(j)%Get_ID() ) Then

                    Do i = 1, Number_of_Branches
                        If ( ( Branch_Type(i)%Get_Zone() == 'C' ) .AND. ( Node_Type(j)%Get_Source() /= Branch_Type(i)%Get_ID() ) ) Then

                                If ( ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_Start() ) .OR. ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_End() ) ) Then

                                    Call Branch_Type(i)%Set_Status(.true.)
                                    Call Zone_C_Reconnect( Branch_Type(i)%Get_ID() ) ! Then enter recursive subroutine with a branch
                            
                                End If

                        End If
                    End Do

                End If
            End Do
            ! _____________ Now to check for branching ... 
            Do j = 1, Number_of_Nodes   
                If ( ( Node_Type(j)%Get_Branching() == .true. ) .AND. ( Node_Type(j)%Get_Zone() == 'C' ) .AND. ( Node_Type(j)%Get_Status() == .true. ) ) Then
                    Do i = 1, Number_of_Branches

                        If ( ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) .AND. ( Branch_Type(i)%Get_Status() == .false. ) ) Then
                            
                            Call Branch_Type(i)%Set_Status(.true.)
                            Call Zone_C_Reconnect( Branch_Type(i)%Get_ID() )

                        End If

                    End Do
                End If
            End Do

        End Subroutine Zone_C_Reconnect
    ! -------------------------------------------------------------------------
        Recursive Subroutine Zone_C_Disconnect (Starting_Element_C) ! This subroutine disconnect the C zone from the previous power supply
            Implicit None                                           

            Integer, Intent(In) :: Starting_Element_C

!            Print *, 'Disconnect rekurzija!', Starting_Element_C

            Do i = 1, Number_of_Branches        ! If Starting_Element is a branch
                If ( Starting_Element_C == Branch_Type(i)%Get_ID() ) Then
                    Do j = 1, Number_of_Nodes
                        
                        If ( Branch_Type(i)%Get_ID() == Node_Type(j)%Get_Source() ) Then

                            Call Branch_Type(i)%Set_Status(.false.)
                            Call Node_Type(j)%Set_Status(.false.)

                            If ( Branch_Type(i)%Get_Zone() == 'Y' ) Then
                                Call Branch_Type(i)%Set_Zone('Z')
                                Call Node_Type(j)%Set_Source( 0 )   ! Disconnecting the node from this branch
                            End If                                  ! If a node is sourced from some branch in zone C, nothing is changed

                            Call Zone_C_Disconnect ( Node_Type(j)%Get_ID() )   ! Then enter recursive subroutine with a node
                        End If

                    End Do
                End If
            End Do
            
            Do j = 1, Number_of_Nodes           ! If Starting_Element is a node
                If ( Starting_Element_C == Node_Type(j)%Get_ID() ) Then
                    Do i = 1, Number_of_Branches

                        If ( ( Branch_Type(i)%Get_Zone() == 'C' ) .AND. ( Node_Type(j)%Get_Source() /= Branch_Type(i)%Get_ID() ) ) Then
                            If ( ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_Start() ) .OR. ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_End() ) ) Then

                                Call Branch_Type(i)%Set_Status(.false.)
                                Call Zone_C_Disconnect( Branch_Type(i)%Get_ID() ) ! Then enter recursive subroutine with a branch
                            
                            End If
                        End If

                    End Do
                End If
            End Do
            ! _____________ Now to check for branching ... 
            Do j = 1, Number_of_Nodes   
                If ( ( Node_Type(j)%Get_Branching() == .true. ) .AND. ( Node_Type(j)%Get_Zone() == 'C' ) .AND. ( Node_Type(j)%Get_Status() == .false. ) ) Then
                    Do i = 1, Number_of_Branches

                        If ( ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) .AND. ( Branch_Type(i)%Get_Status() == .true. ) ) Then
                            
                            Call Branch_Type(i)%Set_Status(.false.)
                            Call Zone_C_Disconnect( Branch_Type(i)%Get_ID() )

                        End If

                    End Do
                End If
            End Do

        End Subroutine Zone_C_Disconnect
    ! -----------------------------------------------------------------------------
        Recursive Subroutine Recursive_Print_Branch (Starting_Element_Br) ! This subroutine prints the branch in the output file
            Implicit None                                           

            Integer, Intent(In) :: Starting_Element_Br
            Integer             :: g, h     ! Iterators

!            Print *, 'Print BRANCH rekurzija!', Starting_Element_Br

            Do i = 1, Number_of_Branches        ! If Starting_Element is a branch and has not yet been printed
                If ( ( Starting_Element_Br == Branch_Type(i)%Get_ID() ) .AND. ( Branch_Type(i)%Get_Status() == .true. ) &
                    .AND. ( Branch_Type(i)%Get_Print() == .false. ) ) Then

                    Write (2, '( "---", 1x, i4, 1x, "---" )', Advance = 'No') Branch_Type(i)%Get_ID()
                    Call Branch_Type(i)%Set_Print(.true.)

                    Do j = 1, Number_of_Nodes
                        If ( Branch_Type(i)%Get_ID() == Node_Type(j)%Get_Source() ) Then                            

                            Call Recursive_Print_Node ( Node_Type(j)%Get_ID() )   ! Then enter recursive subroutine with a node                           
                                                         
                        End If
                    End Do

                End If
            End Do

        End Subroutine Recursive_Print_Branch
    ! -----------------------------------------------------------------------------
        Subroutine Recursive_Print_Node (Starting_Element_N)    ! Subroutine for printing nodes in Result_Output.txt

            Integer, Intent (In) :: Starting_Element_N

!            Print *, 'Print NODE rekurzija!', Starting_Element_N

            Do j = 1, Number_of_Nodes           ! If Starting_Element is a node
                If ( Starting_Element_N == Node_Type(j)%Get_ID() ) Then
                    
                    !If ( Node_Type(j)%Get_Print() == .false. ) Then    ! NISAM SIGURAN DA LI MOZE BEZ OVOGA
                        Write (2, '( "[", 1x, i4, 1x, "]")', Advance = 'No'), Node_Type(j)%Get_ID()
                        Call Node_Type(j)%Set_Print(.true.)
                    !End If

                    Do i = 1, Number_of_Branches                                                 
                        If ( Node_Type(j)%Get_Source() /= Branch_Type(i)%Get_ID() ) Then 
                                                              
                            If ( ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_Start() ) .OR. ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_End() ) ) Then
                                If ( ( Branch_Type(i)%Get_Status() == .true. ) .AND. ( Branch_Type(i)%Get_Print() == .false. ) ) Then

                                    Call Recursive_Print_Branch ( Branch_Type(i)%Get_ID() ) ! Then enter recursive subroutine with a branch

                                End If                                 
                            End If   
                                                         
                        End If
                    End Do                  

                End If
            End Do

        End Subroutine Recursive_Print_Node
    ! -----------------------------------------------------------------------------
        Recursive Subroutine Branching_Print (Starting_Element_B)    !___________Now to check for branching...

            Implicit None                                           

            Integer, Intent(In) :: Starting_Element_B

!            Print *, 'Branching Print rekurzija!', Starting_Element_B

                Do i = 1, Number_of_Branches
                    If ( Starting_Element_B /= Branch_Type(i)%Get_ID() ) Then 

                        If ( ( Starting_Element_B == Branch_Type(i)%Get_Start() ) .OR. ( Starting_Element_B == Branch_Type(i)%Get_End() ) ) Then
                            If ( ( Branch_Type(i)%Get_Status() == .true. ) .AND. ( Branch_Type(i)%Get_Print() == .false. ) ) Then

                                Write (2, '(1x)')                                    
                                Write (2, '( 4x, "| Branching: [", 1x, i4, 1x, "]")', Advance = 'No' ), Starting_Element_B

                                Call Recursive_Print_Branch ( Branch_Type(i)%Get_ID() ) 

                            End If
                        End If

                    End If
                End Do

        End Subroutine Branching_Print
    ! -----------------------------------------------------------------------------

End Module Recursion