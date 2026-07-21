!CACNOTE - All outfld calls to history_out_field are converted
!         - All history_add_field calls are done
!               - Except the constituent history_add_field needs to be changed - see CACNOTE
!               - Need to replace "#" in units?
!         - need to line up calls
!!!----------------------------------------------

module micro_pumas_tempfix


   implicit none
   private

   public :: micro_pumas_tempfix_init ! init routine

CONTAINS

!> \section arg_table_micro_pumas_tempfix_init  Argument Table
!! \htmlinclude micro_pumas_tempfix_init.html
subroutine micro_pumas_tempfix_init(proc_rates, errmsg, errflg)

   use micro_pumas_diags, only: proc_rates_type
   type (proc_rates_type), intent(out)  :: proc_rates


   ! CCPP error handling variables
   character(len=*), intent(out) :: errmsg
   integer,            intent(out) :: errflg

   errmsg = ''
   errflg = 0


end subroutine micro_pumas_tempfix_init

end module micro_pumas_tempfix
