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

!==============================
FUNCTION natoms(this) result(n)
!==============================
    class(StructureState), INTENT(IN) :: this
    INTEGER :: n

    IF (.NOT. ALLOCATED(this%positions_cart)) THEN
        n = 0
    ELSE
        n = SIZE(this%positions_cart,1)
    END IF

END FUNCTION natoms


!==============================
FUNCTION copy(this) result(new_state)
!==============================
    class(StructureState), INTENT(IN) :: this
    TYPE(StructureState) :: new_state

    IF (ALLOCATED(this%lattice)) &
        new_state%lattice = this%lattice

    IF (ALLOCATED(this%positions_cart)) &
        new_state%positions_cart = this%positions_cart

    IF (ALLOCATED(this%positions_frac)) THEN
        new_state%positions_frac = this%positions_frac
        new_state%has_frac = .TRUE.
    END IF

    IF (ALLOCATED(this%TYPE_ids)) &
        new_state%TYPE_ids = this%TYPE_ids

    IF (ALLOCATED(this%species)) &
        new_state%species = this%species

    new_state%energy = this%energy
    new_state%pressure = this%pressure

    IF (ALLOCATED(this%forces)) THEN
        new_state%forces = this%forces
        new_state%has_forces = .TRUE.
    END IF

    IF (ALLOCATED(this%stress)) THEN
        new_state%stress = this%stress
        new_state%has_stress = .TRUE.
    END IF

    new_state%has_energy = this%has_energy

END FUNCTION copy


!==============================
SUBROUTINE validate_shapes(this)
!==============================
    class(StructureState), INTENT(IN) :: this

    INTEGER :: n

    IF (.NOT. ALLOCATED(this%lattice)) THEN
        STOP "lattice NOT ALLOCATED"
    END IF

    IF (SIZE(this%lattice,1) /= 3 .OR. SIZE(this%lattice,2) /= 3) THEN
        STOP "lattice must be (3,3)"
    END IF

    IF (.NOT. ALLOCATED(this%positions_cart)) THEN
        STOP "positions_cart NOT ALLOCATED"
    END IF

    IF (SIZE(this%positions_cart,2) /= 3) THEN
        STOP "positions_cart must be (N,3)"
    END IF

    n = SIZE(this%positions_cart,1)

    IF (.NOT. ALLOCATED(this%TYPE_ids)) THEN
        STOP "TYPE_ids NOT ALLOCATED"
    END IF

    IF (SIZE(this%TYPE_ids) /= n) THEN
        STOP "TYPE_ids wrong length"
    END IF

    IF (ALLOCATED(this%positions_frac)) THEN

        IF (SIZE(this%positions_frac,1) /= n .OR. &
            SIZE(this%positions_frac,2) /= 3) THEN

            STOP "positions_frac must be (N,3)"

        END IF

    END IF

END SUBROUTINE validate_shapes


END module structure_state_module