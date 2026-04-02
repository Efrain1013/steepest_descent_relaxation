MODULE structure_state_module
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: StructureState

    TYPE :: StructureState

        ! ===== geometry =====
        REAL(8), allocatable :: lattice(:,:)        ! (3,3)
        REAL(8), allocatable :: positions_cart(:,:) ! (N,3)
        REAL(8), allocatable :: positions_frac(:,:) ! optional (N,3)
        INTEGER, allocatable :: TYPE_ids(:)         ! (N)
        CHARACTER(len=:), allocatable :: species(:) ! variable-length strINgs

        ! ===== results =====
        REAL(8) :: energy = 0.0d0
        REAL(8), allocatable :: forces(:,:)         ! (N,3)
        REAL(8), allocatable :: stress(:,:)         ! (3,3)
        REAL(8) :: pressure = 0.0d0

        ! ===== flags for optional quantities =====
        LOGICAL :: has_energy = .FALSE.
        LOGICAL :: has_forces = .FALSE.
        LOGICAL :: has_stress = .FALSE.
        LOGICAL :: has_frac  = .FALSE.

    CONTAINS

        procedure :: natoms
        procedure :: copy
        procedure :: validate_shapes

    END TYPE StructureState

CONTAINS

!=========================================
! returns the number of atoms in a state
FUNCTION natoms(self) result(n)
    class(StructureState), INTENT(IN) :: self
    INTEGER :: n
    ! if not allocated assume 0 atoms
    IF (.NOT. ALLOCATED(self%positions_cart)) THEN
        n = 0
    ELSE
        n = SIZE(self%positions_cart,1)
    END IF

END FUNCTION natoms


!============================================
! returns a deep-copy of our StructureState
FUNCTION copy(self) result(new_state)
    class(StructureState), INTENT(IN) :: self
    TYPE(StructureState) :: new_state

    IF (ALLOCATED(self%lattice)) &
        new_state%lattice = self%lattice

    IF (ALLOCATED(self%positions_cart)) &
        new_state%positions_cart = self%positions_cart

    IF (ALLOCATED(self%positions_frac)) THEN
        new_state%positions_frac = self%positions_frac
        new_state%has_frac = .TRUE.
    END IF

    IF (ALLOCATED(self%TYPE_ids)) &
        new_state%TYPE_ids = self%TYPE_ids

    IF (ALLOCATED(self%species)) &
        new_state%species = self%species

    new_state%energy = self%energy
    new_state%pressure = self%pressure

    IF (ALLOCATED(self%forces)) THEN
        new_state%forces = self%forces
        new_state%has_forces = .TRUE.
    END IF

    IF (ALLOCATED(self%stress)) THEN
        new_state%stress = self%stress
        new_state%has_stress = .TRUE.
    END IF

    new_state%has_energy = self%has_energy

END FUNCTION copy


!===============================================
! does basic consistency checks for the states
SUBROUTINE validate_shapes(self)
    class(StructureState), INTENT(IN) :: self

    INTEGER :: n

    IF (.NOT. ALLOCATED(self%lattice)) THEN
        STOP "lattice NOT ALLOCATED"
    END IF
    IF (SIZE(self%lattice,1) /= 3 .OR. SIZE(self%lattice,2) /= 3) THEN
        STOP "lattice must be (3,3)"
    END IF

    IF (.NOT. ALLOCATED(self%positions_cart)) THEN
        STOP "positions_cart NOT ALLOCATED"
    END IF

    IF (SIZE(self%positions_cart,2) /= 3) THEN
        STOP "positions_cart must be (N,3)"
    END IF

    n = SIZE(self%positions_cart,1)

    IF (.NOT. ALLOCATED(self%TYPE_ids)) THEN
        STOP "TYPE_ids NOT ALLOCATED"
    END IF

    IF (SIZE(self%TYPE_ids) /= n) THEN
        STOP "TYPE_ids wrong length"
    END IF

    IF (ALLOCATED(self%positions_frac)) THEN

        IF (SIZE(self%positions_frac,1) /= n .OR. &
            SIZE(self%positions_frac,2) /= 3) THEN

            STOP "positions_frac must be (N,3)"

        END IF
    END IF

END SUBROUTINE validate_shapes


END module structure_state_module