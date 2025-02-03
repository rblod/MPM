!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. You can use, modify, and/or redistribute it under the
! terms of the CeCILL-C license. For details, see: http://www.cecill.info.
!===============================================================================

module mod_precision
  implicit none
#ifdef USE_DOUBLE
  integer, parameter :: wp = kind(1.0d0)
#else
  integer, parameter :: wp = kind(1.0)
#endif
  ! For convenience, also define sp and dp:
  integer, parameter :: sp = kind(1.0)
  integer, parameter :: dp = kind(1.0d0)
end module mod_precision