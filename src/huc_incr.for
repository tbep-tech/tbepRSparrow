C HUC_INCR.FOR
C  Purpose: 
C   Designed to fill-in 523 "CANADIAN" reaches w/o HUC8 using value from nearest downstream reach
C  Notes:
C   -reaches sorted by HYDSEQ
C   -input: 'huc8' - reach HUC8 value
C   -returns: 'nhuc8' - new reach HUC8 value
   
      subroutine huc_incr(nreach,nnode,fnode,tnode,staid,nstaid)
        !GCC$ ATTRIBUTES DLLEXPORT::huc_incr
        integer, intent(in) :: nreach,nnode
        integer, intent(in) :: fnode(nreach),tnode(nreach)
        integer, intent(in) :: staid(nreach)
        integer, intent(inout) :: nstaid(nreach)
        integer :: nmont(nnode)

        do i=1,nnode
         nmont(i) = 0
        end do

        do i=nreach,1,-1
          if(staid(i).gt.0) then
             nmont(fnode(i)) = staid(i)  ! assign new HUC8 to upstream node
             nstaid(i) = staid(i)
          else
             nmont(fnode(i)) = nmont(tnode(i)) ! pass HUC8 upstream
             nstaid(i) = nmont(tnode(i))
          endif
        end do
        end subroutine huc_incr
