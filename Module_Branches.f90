Module Branches
    
    Use Elements
    Implicit None
 !==================================================================!
    Type, Public, Extends (Element) :: Branch

        Private
            Integer :: Start_Node   ! The beginning of a branch
            Integer :: End_Node     ! Branch ending node index
            Logical :: Reserve_Branch   ! If the branch is a reserve power source for a node

        Contains
            Procedure, Public :: Set_Start   => Set_Start_Node
            Procedure, Public :: Set_End     => Set_End_Node
            Procedure, Public :: Set_Reserve => Set_Reserve_Branch

            Procedure, Public :: Get_Start   => Get_Start_Node
            Procedure, Public :: Get_End     => Get_End_Node
            Procedure, Public :: Get_Reserve => Get_Reserve_Branch

    End Type Branch
!==================================================================!
    Private :: Set_Start_Node, Set_End_Node, Set_Reserve_Branch
    Private :: Get_Start_Node, Get_End_Node, Get_Reserve_Branch

    Contains !=======================================================

        Subroutine Set_Start_Node (this, Start)
            Implicit None

            Class (Branch) :: this
            Integer      :: Start

            this%Start_Node = Start

        End Subroutine Set_Start_Node
    !------------------------------------------------
        Subroutine Set_End_Node (this, End_n) ! END is a fortran command,
            Implicit None                     ! so we use End_n instead

            Class (Branch) :: this
            Integer      :: End_n

            this%End_Node = End_n

        End Subroutine Set_End_Node
    !------------------------------------------------
        Subroutine Set_Reserve_Branch (this, Reserve)
            Implicit None

            Class (Branch) :: this
            Logical        :: Reserve

            this%Reserve_Branch = Reserve

        End Subroutine Set_Reserve_Branch
    !------------------------------------------------
        Integer Function Get_Start_Node (this)
            Implicit None

            Class (Branch) :: this

            Get_Start_Node = this%Start_Node

        End Function Get_Start_Node
    !------------------------------------------------
        Integer Function Get_End_Node (this)
            Implicit None

            Class (Branch) :: this

            Get_End_Node = this%End_Node

        End Function Get_End_Node
    !------------------------------------------------
        Logical Function Get_Reserve_Branch (this)
            Implicit None

            Class (Branch) :: this

            Get_Reserve_Branch = this%Reserve_Branch

        End Function Get_Reserve_Branch
    !------------------------------------------------

End Module Branches