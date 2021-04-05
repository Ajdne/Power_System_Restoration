Module Load_Grid

    Use Nodes
    Use Branches
    Implicit None

    Integer :: i, j, k, m, n   ! Iterators
    Integer :: Number_of_Nodes
    Integer :: Number_of_Branches
    Integer :: Short_Circuit_Location

    Type (Node), Allocatable   :: Node_type(:)
    Type (Branch), Allocatable :: Branch_type(:)

    ! ___ Arrays needed to read data from .txt file ___
    Integer   :: Element_ID    
    Logical   :: Element_Status 
    Integer   :: Source_Branch  
    Integer   :: Start_Node     
    Integer   :: End_Node       
  
!=============================================================
    Contains 
        
        Subroutine Load_Grid_Data
            Implicit None

            Open (1, File = "Load_Grid_Data.txt") !File status is 'Unknown' by default
                Read (1, *)
                Read (1, *)     ! Need this to skip lines in .txt file
                Read (1, *) Number_of_Nodes
                Read (1, *)
                Read (1, *)
                Read (1, *)
                Read (1, *) Number_of_Branches
                Read (1, *)
                Read (1, *)
                Read (1, *)
                Read (1, *) Short_Circuit_Location

                Do i = 1, 5     ! Skipping some empty space...
                    Read (1, *) 
                End Do

                Allocate (Node_type(Number_of_Nodes))

                Do i = 1, Number_of_Nodes
                    Read (1, '(3x, i4, 5x, i4)') Element_ID, Source_Branch

                    Call Node_type(i)%Set_ID (Element_ID) ! This subroutine is from Element class
                    Call Node_type(i)%Set_Source (Source_Branch)
                    Call Node_type(i)%Set_Print (.false.)   ! Need to set all elements to NOT printed

                End Do      

                Do i = 1, 5     ! Skip
                    Read (1, *)     ! Skip
                End Do                  ! Skip

                Allocate (Branch_type(Number_of_Branches))

                Do i = 1, Number_of_Branches
                    Read (1, '(3x, i4, 4x, i4, 2x, i4)') Element_ID, Start_Node, End_Node

                    Call Branch_type(i)%Set_ID (Element_ID)      ! Subroutine from Element class

                    Call Branch_type(i)%Set_Start (Start_Node)   ! Subroutines from Branch class
                    Call Branch_type(i)%Set_End (End_Node)
                    Call Branch_type(i)%Set_Print (.false.)
                                                            
                End Do

            Close (1)

        End Subroutine Load_Grid_Data
!-----------------------------------------------------------------------------------------------
End Module Load_Grid