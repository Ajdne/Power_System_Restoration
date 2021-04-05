Module Elements
    Implicit None

 !==================================================================!
    Type, Public :: Element

        Private
            Integer   :: Element_ID
            Logical   :: Element_Status ! On/Off state
            Character :: Element_Zone   ! A, B or C zone - needed for power restoration algorithm
            Logical   :: Element_Print  ! This is needed for result output

        Contains
            Procedure, Public :: Set_ID     => Set_Element_ID
            Procedure, Public :: Set_Status => Set_Element_Status
            Procedure, Public :: Set_Zone   => Set_Element_Zone
            Procedure, Public :: Set_Print  => Set_Element_Print

            Procedure, Public :: Get_ID     => Get_Element_ID
            Procedure, Public :: Get_Status => Get_Element_Status
            Procedure, Public :: Get_Zone   => Get_Element_Zone
            Procedure, Public :: Get_Print  => Get_Element_Print

    End Type Element
    !==============================================================

    Private :: Set_Element_ID, Set_Element_Status, Set_Element_Zone, Set_Element_Print
    Private :: Get_Element_ID, Get_Element_Status, Get_Element_Zone, Get_Element_Print

    Contains !======================================================

        Subroutine Set_Element_ID (this, ID)
            Implicit None

            Class (Element) :: this
            Integer         :: ID

            this%Element_ID = ID

        End Subroutine Set_Element_ID
    !------------------------------------------------
        Subroutine Set_Element_Status (this, Status)
            Implicit None

            Class (Element) :: this
            Logical         :: Status

            this%Element_Status = Status

        End Subroutine Set_Element_Status
    !------------------------------------------------
        Subroutine Set_Element_Zone (this, Zone)
            Implicit None

            Class (Element) :: this
            Character       :: Zone

            this%Element_Zone = Zone

        End Subroutine Set_Element_Zone
    !------------------------------------------------
        Subroutine Set_Element_Print (this, Printed)
            Implicit None

            Class (Element) :: this
            Logical         :: Printed

            this%Element_Print = Printed

        End Subroutine Set_Element_Print
    !------------------------------------------------
        Integer Function Get_Element_ID (this)
            Implicit None

            Class (Element) :: this

            Get_Element_ID = this%Element_ID

        End Function Get_Element_ID
    !------------------------------------------------
        Logical Function Get_Element_Status (this)
            Implicit None

            Class (Element) :: this

            Get_Element_Status = this%Element_Status

        End Function Get_Element_Status
    !------------------------------------------------
        Character Function Get_Element_Zone (this)
            Implicit None

            Class (Element) :: this

            Get_Element_Zone = this%Element_Zone

        End Function Get_Element_Zone
    !------------------------------------------------
        Logical Function Get_Element_Print (this)
            Implicit None

            Class (Element) :: this

            Get_Element_Print = this%Element_Print

        End Function Get_Element_Print
    !------------------------------------------------
End Module Elements