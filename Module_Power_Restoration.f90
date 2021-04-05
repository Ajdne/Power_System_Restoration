Module Power_Restoration

    Use Load_Grid
    Implicit None

    Integer, Private :: B_Counter
    Integer          :: X_Counter

    Contains !==================================================
        
        Subroutine Find_Short_Circuit   ! And defining 'B' zone
            Implicit None

            Do i = 1, Number_of_Branches 
                              
                If ( Short_Circuit_Location == Branch_type(i)%Get_ID() ) Then

                    Call Branch_type(i)%Set_Status(.false.) ! Changing the status of the element to 'not working'
                    Call Branch_Type(i)%Set_Zone('B')       ! Zone 'B' is the malfunctioning ('broken') zone

                End If
            End Do

        End Subroutine Find_Short_Circuit
    ! -------------------------------------------------------------------------
        Subroutine Find_Reserve_Branches   ! Finding branches that are currently not connected to any node,
            Implicit None                  ! but could be at some point (they are "free" - unused)

            Do i = 1, Number_of_Branches 
                Do j = 1, Number_of_Nodes  
                                
                    If ( ( Node_Type(j)%Get_ID() == Branch_type(i)%Get_End() ) .AND. ( Branch_type(i)%Get_ID() /= Node_Type(j)%Get_Source() ) ) Then

                        Call Branch_type(i)%Set_Reserve(.true.) ! This branch can be a reserve source for node(j)
                        Call Branch_Type(i)%Set_Status(.false.) ! and is currently not in use, so we turn it off
                        Exit    ! Exit the Node DO loop and proceed to the next branch
                    Else
                        Call Branch_type(i)%Set_Reserve(.false.)    ! The branch is not a reserve branch
                        Call Branch_Type(i)%Set_Status(.true.)      ! and is currently in use
                    End If

                End Do
            End Do

        End Subroutine Find_Reserve_Branches
    ! -------------------------------------------------------------------------
        Subroutine Find_Branching   ! Find nodes that are a starting node for 2 or more branches
            Implicit None           ! This is necessary for later subroutines
                
                Do j = 1, Number_of_Nodes

                    Do i = 1, Number_of_Branches

                        If ( ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_Start() ) .OR. ( Node_Type(j)%Get_ID() == Branch_Type(i)%Get_End() ) ) Then
                            B_Counter = B_Counter + 1   ! B_Counter is 0 by default
                        End If

                    End Do

                    If (B_Counter > 2) Then     ! Regular nodes have 1 for branch end and 1 for branch start. Branching nodes have more than 2...
                        Call Node_Type(j)%Set_Branching(.true.) ! Now the nodes that are branching are marked
                    Else
                        Call Node_Type(j)%Set_Branching(.false.)
                    End If                                  

                    B_Counter = 0     ! Need to reset counter when switching to another node
                End Do

        End Subroutine Find_Branching
    ! -------------------------------------------------------------------------
        Subroutine Restoring_Zone_A
            Implicit None

                Do i = 1, Number_of_Branches
                    If (Branch_Type(i)%Get_Zone() == 'A') Then  ! If an element is in zone A
                        Call Branch_Type(i)%Set_Status(.true.)  ! the power is restored
                    End If
                End Do

                Do i = 1, Number_of_Nodes
                    If (Node_Type(i)%Get_Zone() == 'A') Then
                        Call Node_Type(i)%Set_Status(.true.)
                    End If
                End Do

        End Subroutine Restoring_Zone_A
    ! -------------------------------------------------------------------------
        Subroutine C_Zone_Potentials    ! Finding branches that can power C zone
            Implicit None               

            Do i = 1, Number_of_Branches
                If ( ( Branch_Type(i)%Get_Reserve() == .true. ) .AND. ( Branch_Type(i)%Get_Zone() /= 'B' ) ) Then
                                                    ! Finding the branch that is not in B zone
                    Do j = 1, Number_of_Nodes

                         If ( Node_Type(j)%Get_Zone() == 'C' ) Then
                            If ( Node_Type(j)%Get_Source() /= Branch_Type(i)%Get_ID() ) Then 
                                                                  
                                If ( ( Branch_Type(i)%Get_End() == Node_Type(j)%Get_ID() ) .OR. ( Branch_Type(i)%Get_Start() == Node_Type(j)%Get_ID() ) ) Then
                                    Call Branch_Type(i)%Set_Zone('X')   ! If branch(i) has an ending or beginning in node(j) and is not its source branch
                                    X_Counter = X_Counter + 1
                                End If

                            End If
                        End If

                    End Do
                End If
            End Do

        End Subroutine C_Zone_Potentials
    ! -------------------------------------------------------------------------
        Subroutine Connecting_Zone_C
            Implicit None

            Zone_C_Source_fix: Do i = 1, Number_of_Branches ! If the node is not powered from any source,
                Do j = 1, Number_of_Nodes                   ! then change its source to branch in zone C

                    If ( (Branch_Type(i)%Get_Zone() == 'C') .AND. (Node_Type(j)%Get_Source() == 0) ) Then

                        If ( (Node_Type(j)%Get_ID() == Branch_Type(i)%Get_End()) .OR. (Node_Type(j)%Get_ID() == Branch_Type(i)%Get_Start()) ) Then
                            
                            Call Node_Type(j)%Set_Source( Branch_Type(i)%Get_ID() )
                            Call Node_Type(j)%Set_Status(.true.)
                            Call Branch_Type(i)%Set_Status(.true.)

                        End If
                    End If
                End Do
            End Do Zone_C_Source_Fix

            Outer: Do i = 1, Number_of_Branches
                Inner: Do j = 1, Number_of_Nodes

                    If (Branch_Type(i)%Get_Zone() == 'X') Then

                        If ( (Node_Type(j)%Get_ID() == Branch_Type(i)%Get_End() ) .AND. (Node_Type(j)%Get_Source() /= Branch_Type(i)%Get_ID()) ) Then
                            
                            Call Node_Type(j)%Set_Source( Branch_Type(i)%Get_ID() )
                            Call Node_Type(j)%Set_Status(.true.)
                            Call Branch_Type(i)%Set_Zone('Y')   ! Using 'Y' to mark the "used" branch connections to C zone

                            Exit Outer  ! Terminating the loop process altogether, since the solution is found

                        End If 

                    End If

                End Do Inner
            End Do Outer

        End Subroutine Connecting_Zone_C
    ! -------------------------------------------------------------------------
        Subroutine Disconnecting_Zone_C
            Implicit None       
                    
            Do i = 1, Number_of_Branches
                Do j = 1, Number_of_Nodes                       
                    
                    If ( Node_Type(j)%Get_Zone() == 'C') Then
                        
                        Call Node_Type(j)%Set_Status(.false.)

                        If ( (Branch_Type(i)%Get_Zone() == 'Y') .AND. (Node_Type(j)%Get_Source() == Branch_Type(i)%Get_ID() ) ) Then
                                                                ! 'Y' goes for connected branches
                            Call Node_Type(j)%Set_Source(0)
                            Call Branch_Type(i)%Set_Zone('Z')       ! 'Z' goes for disconnected, after being previously connected ('Y')
                                                                    ! and 'X' is for possible connections
                        End If
                    End If

                    If ( Branch_Type(i)%Get_Zone() == 'C') Then
                        Call Branch_Type(i)%Set_Status(.false.)
                    End If

                End Do
            End Do

        End Subroutine Disconnecting_Zone_C
    ! -------------------------------------------------------------------------
End Module Power_Restoration