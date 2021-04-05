Module Load_Parameters

    Implicit None

    Integer :: Defective_Element_Index

    !_______ 1. Unrestored Load Power _____________
    Integer :: p, q     ! Weight factors for regular and prioritised consumer nodes

    !_______ 2. Manipulation Costs _____________
    Real    :: c        ! Cost of a single manipulation [r.u.]

    Contains !==================================================

        Subroutine Load_Setup_Parameters
            Implicit None                               
                                                                    ! Status = 'Old' means that opening the .txt file is possible
            Open (2, File = 'Load_Parameters.txt', Status = 'Old') ! only if that file exists in the folder
                Read(2, *)                                          ! Default status value is 'Unknown' which means that the file will be
                Read (2, *) Defective_Element_Index                 ! created if it does not exist, or just opened if it does exist
                Read (2, *)
                Read (2, *)          ! Empty space in 'Setup_Parameters.txt'
                Read (2, *) p
                Read (2, *) q  
                Read (2, *)
                Read (2, *)
                Read (2, *) c


            Close (2)

        End Subroutine Load_Setup_Parameters






End Module Load_Parameters