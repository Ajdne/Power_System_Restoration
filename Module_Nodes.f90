Module Nodes
    
    Use Elements
    Implicit None
 !==================================================================!
    Type, Public, Extends (Element) :: Node

        Private
            Integer :: Source_Branch    ! Branch index from which this node is powered
            Logical :: Is_Branching     ! If 2 or more branches leave this node, then the node is branching

        Contains
            Procedure, Public :: Set_Source    => Set_Source_Branch
            Procedure, Public :: Set_Branching => Set_Node_Branching

            Procedure, Public :: Get_Source    => Get_Source_Branch
            Procedure, Public :: Get_Branching => Get_Node_Branching

    End Type Node
!==================================================================

    Private :: Set_Source_Branch, Set_Node_Branching
    Private :: Get_Source_Branch, Get_Node_Branching

    Contains !=======================================================

        Subroutine Set_Source_Branch (this, Source)
            Implicit None

            Class (Node) :: this
            Integer      :: Source

            this%Source_Branch = Source

        End Subroutine Set_Source_Branch
    !------------------------------------------------
        Integer Function Get_Source_Branch (this)
            Implicit None

            Class (Node) :: this

            Get_Source_Branch = this%Source_Branch

        End Function Get_Source_Branch
    !------------------------------------------------
        Subroutine Set_Node_Branching (this, Branching)
            Implicit None

            Class (Node) :: this
            Logical      :: Branching

            this%Is_Branching = Branching

        End Subroutine Set_Node_Branching
    !----------------------------------------------
        Logical Function Get_Node_Branching (this)
            Implicit None

            Class (Node) :: this

            Get_Node_Branching = this%Is_Branching

        End Function Get_Node_Branching
    !------------------------------------------------
End Module Nodes