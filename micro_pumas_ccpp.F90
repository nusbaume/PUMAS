!Common Community Physics Package (CCPP) wrapper for PUMAS.
module micro_pumas_ccpp

  implicit none
  private
  save

  public :: micro_pumas_ccpp_init
  public :: micro_pumas_ccpp_run

contains

  !> \section arg_table_micro_pumas_ccpp_init Argument Table
  !! \htmlinclude micro_pumas_ccpp_init.html
  subroutine micro_pumas_ccpp_init(gravit, rair, rh2o, cpair, tmelt, latvap, latice,     &
                                   rhmini, iulog, micro_mg_do_hail, micro_mg_do_graupel, &
                                   microp_uniform, do_cldice, use_hetfrz_classnuc,       &
                                   remove_supersat, micro_mg_evap_sed_off,               &
                                   micro_mg_icenuc_rh_off, micro_mg_icenuc_use_meyers,   &
                                   micro_mg_evap_scl_ifs, micro_mg_evap_rhthrsh_ifs,     &
                                   micro_mg_rainfreeze_ifs, micro_mg_ifs_sed,            &
                                   micro_mg_precip_fall_corr, micro_mg_accre_sees_auto,  &
                                   micro_mg_implicit_fall, micro_mg_nccons,              &
                                   micro_mg_nicons, micro_mg_ngcons, micro_mg_nrcons,    &
                                   micro_mg_nscons, micro_mg_precip_frac_method,         &
                                   micro_mg_warm_rain,                                   &
                                   stochastic_emulated_filename_quantile,                &
                                   stochastic_emulated_filename_input_scale,             &
                                   stochastic_emulated_filename_output_scale,            &
                                   micro_mg_dcs_in,                                      &
                                   micro_mg_berg_eff_factor_in, micro_mg_accre_enhan_fact_in, &
                                   micro_mg_autocon_fact_in, micro_mg_autocon_nd_exp_in,      &
                                   micro_mg_autocon_lwp_exp_in, micro_mg_homog_size_in,       &
                                   micro_mg_vtrmi_factor_in,    micro_mg_vtrms_factor_in,     &
                                   micro_mg_effi_factor_in,     micro_mg_iaccr_factor_in,     &
                                   micro_mg_max_nicons_in, micro_mg_ncnst_in,                 &
                                   micro_mg_ninst_in, micro_mg_ngnst_in, micro_mg_nrnst_in,   &
                                   micro_mg_nsnst_in, errmsg, errcode)

  !External dependencies:
  use ccpp_kinds,        only: kind_phys
  use micro_pumas_v1,    only: micro_pumas_init
  use pumas_kinds,       only: pumas_r8=>kind_r8

  !Subroutine (dummy) arguments:

  !Host model constants:
  real(kind_phys), intent(in) :: gravit !standard gravitational acceleration                    (m s-2)
  real(kind_phys), intent(in) :: rair   !gas constant for dry air                               (J kg-1 K-1)
  real(kind_phys), intent(in) :: rh2o   !gas constat for water vapor                            (J kg-1 K-1)
  real(kind_phys), intent(in) :: cpair  !specific heat of dry air at constant pressure          (J kg-1 K-1)
  real(kind_phys), intent(in) :: tmelt  !freezing point of water                                (K)
  real(kind_phys), intent(in) :: latvap !latent heat of vaporization of water at 0 degrees C    (J kg-1)
  real(kind_phys), intent(in) :: latice !latent heat of fusion of water at 0 degrees C          (J kg-1)
  real(kind_phys), intent(in) :: rhmini !Minimum RH for ice cloud fraction > 0                  (fraction)

  !Host model variables:
  integer, intent(in) :: iulog          !Log output unit number (1)

  !PUMAS-specific parameters:
  !-------------------------
  logical, intent(in) :: micro_mg_do_hail           !flag for PUMAS to simulate hail                              (flag)
  logical, intent(in) :: micro_mg_do_graupel        !flag for PUMAS to simulate graupel                           (flag)
  logical, intent(in) :: microp_uniform             !flag for PUMAS to perform uniform calc.                      (flag)
  logical, intent(in) :: do_cldice                  !flag for PUMAS to simulate cloud ice                         (flag)
  logical, intent(in) :: use_hetfrz_classnuc        !flag to turn on PUMAS heterogeneous freezing                 (flag)
  logical, intent(in) :: remove_supersat            !flag to remove supersaturation after sedimentation loop      (flag)
  logical, intent(in) :: micro_mg_evap_sed_off      !flag to turn off condensate evap. after sedimentation        (flag)
  logical, intent(in) :: micro_mg_icenuc_rh_off     !flag to turn off RH threshold for ice nucleation             (flag)
  logical, intent(in) :: micro_mg_icenuc_use_meyers !flag to use Meyers 1992 temp. dependent ice nucleation       (flag)
  logical, intent(in) :: micro_mg_evap_scl_ifs      !flag to apply IFS precipitation evap. scaling                (flag)
  logical, intent(in) :: micro_mg_evap_rhthrsh_ifs  !flag to use IFS precipitation evap. RH threshold             (flag)
  logical, intent(in) :: micro_mg_rainfreeze_ifs    !flag to freeze rain at 0 degrees C as is done in IFS         (flag)
  logical, intent(in) :: micro_mg_ifs_sed           !flag to use IFS sedimentation fall speeds                    (flag)
  logical, intent(in) :: micro_mg_precip_fall_corr  !flag to ensure non-zero precip fall speed if precip above    (flag)
  logical, intent(in) :: micro_mg_accre_sees_auto   !flag to add autoconverted liuqid to rain before accretion    (flag)
  logical, intent(in) :: micro_mg_implicit_fall     !flag to use implicit fall speed routine for all hydrometeors (flag)
  logical, intent(in) :: micro_mg_nccons            !flag to have PUMAS hold cloud droplet number constant        (flag)
  logical, intent(in) :: micro_mg_nicons            !flag to have PUMAS hold cloud ice number constant            (flag)
  logical, intent(in) :: micro_mg_ngcons            !flag to have PUMAS hold cloud graupel number constant        (flag)
  logical, intent(in) :: micro_mg_nrcons            !flag to have PUMAS hold cloud rain number constant           (flag)
  logical, intent(in) :: micro_mg_nscons            !flag to have PUMAS hold cloud snow number constant           (flag)

  !type of precipitation fraction method (none):
  character(len=*), intent(in) :: micro_mg_precip_frac_method
  !type of warm rain autoconversion/accr.method to use (none):
  character(len=*), intent(in) :: micro_mg_warm_rain
  !neural net file for warm_rain machine learning (none):
  character(len=*), intent(in) :: stochastic_emulated_filename_quantile
  !neural net input scaling values files for warm_rain machine learning (none):
  character(len=*), intent(in) :: stochastic_emulated_filename_input_scale
  !Neural net output scaling values file for warm_rain machine learning (none):
  character(len=*), intent(in) :: stochastic_emulated_filename_output_scale

  real(kind_phys), intent(in) :: micro_mg_dcs_in              !autoconversion size threshold                      (um)
  real(kind_phys), intent(in) :: micro_mg_berg_eff_factor_in  !efficienty factor for Bergeron process             (1)
  real(kind_phys), intent(in) :: micro_mg_accre_enhan_fact_in !accretion enhancement factor                       (1)
  real(kind_phys), intent(in) :: micro_mg_autocon_fact_in     !autoconverion enhancement prefactor                (1)
  real(kind_phys), intent(in) :: micro_mg_autocon_nd_exp_in   !autconversion cloud liquid exponent factor         (1)
  real(kind_phys), intent(in) :: micro_mg_autocon_lwp_exp_in  !autoconversion LWP exponent factor                 (1)
  real(kind_phys), intent(in) :: micro_mg_homog_size_in       !mean volume radius of homoegenous freezing ice     (m)
  real(kind_phys), intent(in) :: micro_mg_vtrmi_factor_in     !ice fall velocity enhancement factor               (1)
  real(kind_phys), intent(in) :: micro_mg_vtrms_factor_in     !snow fall velocity enhancement factor              (1)
  real(kind_phys), intent(in) :: micro_mg_effi_factor_in      !ice effective radius enhancement factor            (1)
  real(kind_phys), intent(in) :: micro_mg_iaccr_factor_in     !ice accretion factor                               (1)
  real(kind_phys), intent(in) :: micro_mg_max_nicons_in       !max allowed ice number concentration               (m-3)

  !In-cloud droplet number concentration if micro_mg_nccons is True (m-3):
  real(kind_phys), intent(in) :: micro_mg_ncnst_in
  !In-cloud ice number concentration if micro_mg_nicons is True     (m-3):
  real(kind_phys), intent(in) :: micro_mg_ninst_in
  !In-cloud graupel number concentration if micro_mg_ngcons is True (m-3):
  real(kind_phys), intent(in) :: micro_mg_ngnst_in
  !In-cloud rain number concentration when micro_mg_nrcons is True  (m-3):
  real(kind_phys), intent(in) :: micro_mg_nrnst_in
  !In-cloud snow number concentration when micro_mg_nscons is True  (m-3):
  real(kind_phys), intent(in) :: micro_mg_nsnst_in
  !-------------------------

  !Output variables:
  character(len=512), intent(out) :: errmsg  !PUMAS/CCPP error message (none)
  integer,            intent(out) :: errcode !CCPP error code (1)

  !Local variables:
  real(pumas_r8) :: micro_mg_dcs              !autoconversion size threshold                      (um)
  real(pumas_r8) :: micro_mg_berg_eff_factor  !efficienty factor for Bergeron process             (1)
  real(pumas_r8) :: micro_mg_accre_enhan_fact !accretion enhancement factor                       (1)
  real(pumas_r8) :: micro_mg_autocon_fact     !autoconverion enhancement prefactor                (1)
  real(pumas_r8) :: micro_mg_autocon_nd_exp   !autconversion cloud liquid exponent factor         (1)
  real(pumas_r8) :: micro_mg_autocon_lwp_exp  !autoconversion LWP exponent factor                 (1)
  real(pumas_r8) :: micro_mg_homog_size       !mean volume radius of homoegenous freezing ice     (m)
  real(pumas_r8) :: micro_mg_vtrmi_factor     !ice fall velocity enhancement factor               (1)
  real(pumas_r8) :: micro_mg_vtrms_factor     !snow fall velocity enhancement factor              (1)
  real(pumas_r8) :: micro_mg_effi_factor      !ice effective radius enhancement factor            (1)
  real(pumas_r8) :: micro_mg_iaccr_factor     !ice accretion factor                               (1)
  real(pumas_r8) :: micro_mg_max_nicons       !max allowed ice number concentration               (m-3)

  !In-cloud droplet number concentration if micro_mg_nccons is True (m-3):
  real(pumas_r8) :: micro_mg_ncnst
  !In-cloud ice number concentration if micro_mg_nicons is True     (m-3):
  real(pumas_r8) :: micro_mg_ninst
  !In-cloud graupel number concentration if micro_mg_ngcons is True (m-3):
  real(pumas_r8) :: micro_mg_ngnst
  !In-cloud rain number concentration when micro_mg_nrcons is True  (m-3):
  real(pumas_r8) :: micro_mg_nrnst
  !In-cloud snow number concentration when micro_mg_nscons is True  (m-3):
  real(pumas_r8) :: micro_mg_nsnst

  !Local PUMAS error message
  character(len=128) :: pumas_errstring

  !Initialize error message and error code:
  errmsg  = ''
  errcode = 0

  !Convert real-type input fields into appropriate kind:
  micro_mg_dcs              = real(micro_mg_dcs_in, pumas_r8)
  micro_mg_berg_eff_factor  = real(micro_mg_berg_eff_factor_in, pumas_r8)
  micro_mg_accre_enhan_fact = real(micro_mg_accre_enhan_fact_in, pumas_r8)
  micro_mg_autocon_fact     = real(micro_mg_autocon_fact_in, pumas_r8)
  micro_mg_autocon_nd_exp   = real(micro_mg_autocon_nd_exp_in, pumas_r8)
  micro_mg_autocon_lwp_exp  = real(micro_mg_autocon_lwp_exp_in, pumas_r8)
  micro_mg_homog_size       = real(micro_mg_homog_size_in, pumas_r8)
  micro_mg_vtrmi_factor     = real(micro_mg_vtrmi_factor_in, pumas_r8)
  micro_mg_vtrms_factor     = real(micro_mg_vtrms_factor_in, pumas_r8)
  micro_mg_effi_factor      = real(micro_mg_effi_factor_in, pumas_r8)
  micro_mg_iaccr_factor     = real(micro_mg_iaccr_factor_in, pumas_r8)
  micro_mg_max_nicons       = real(micro_mg_max_nicons_in, pumas_r8)
  micro_mg_ncnst            = real(micro_mg_ncnst_in, pumas_r8)
  micro_mg_ninst            = real(micro_mg_ninst_in, pumas_r8)
  micro_mg_ngnst            = real(micro_mg_ngnst_in, pumas_r8)
  micro_mg_nrnst            = real(micro_mg_nrnst_in, pumas_r8)
  micro_mg_nsnst            = real(micro_mg_nsnst_in, pumas_r8)

  !Call PUMAS initialization routine:
  call micro_pumas_init( &
           pumas_r8, gravit, rair, rh2o, cpair, &
           tmelt, latvap, latice, rhmini, &
           micro_mg_dcs,                  &
           micro_mg_do_hail,micro_mg_do_graupel, &
           microp_uniform, do_cldice, use_hetfrz_classnuc, &
           micro_mg_precip_frac_method, micro_mg_berg_eff_factor, &
           micro_mg_accre_enhan_fact , &
           micro_mg_autocon_fact , micro_mg_autocon_nd_exp, micro_mg_autocon_lwp_exp, micro_mg_homog_size, &
           micro_mg_vtrmi_factor, micro_mg_vtrms_factor, micro_mg_effi_factor, &
           micro_mg_iaccr_factor, micro_mg_max_nicons, &
           remove_supersat, micro_mg_warm_rain, &
           micro_mg_evap_sed_off, micro_mg_icenuc_rh_off, micro_mg_icenuc_use_meyers, &
           micro_mg_evap_scl_ifs, micro_mg_evap_rhthrsh_ifs, &
           micro_mg_rainfreeze_ifs,  micro_mg_ifs_sed, micro_mg_precip_fall_corr,&
           micro_mg_accre_sees_auto, micro_mg_implicit_fall, &
           micro_mg_nccons, micro_mg_nicons, micro_mg_ncnst, &
           micro_mg_ninst, micro_mg_ngcons, micro_mg_ngnst, &
           micro_mg_nrcons, micro_mg_nrnst, micro_mg_nscons, micro_mg_nsnst, &
           stochastic_emulated_filename_quantile, stochastic_emulated_filename_input_scale, &
           stochastic_emulated_filename_output_scale, iulog, pumas_errstring)

  !Set error code to non-zero value if PUMAS returns an error message:
  if (trim(pumas_errstring) /= "") then
    errcode = 1
    errmsg = trim(pumas_errstring)
  end if

  end subroutine micro_pumas_ccpp_init

  !> \section arg_table_micro_pumas_ccpp_run Argument Table
  !! \htmlinclude micro_pumas_ccpp_run.html
  subroutine micro_pumas_ccpp_run(micro_ncol, micro_nlev, micro_nlevp1,             &
                                  micro_dust_nbins, pumas_timestep,              &
                                  pumas_airT, pumas_airq, pumas_cldliq,    &
                                  pumas_cldice,   pumas_numliq,               &
                                  pumas_numice,   pumas_rainliq,              &
                                  pumas_snowice,  pumas_numrain,              &
                                  pumas_numsnow,  pumas_graupice,             &
                                  pumas_numgraup, pumas_relvar,               &
                                  pumas_accre_enhan, pumas_pmid,              &
                                  pumas_pdel, pumas_pint,                     &
                                  pumas_strat_cldfrc, pumas_strat_liq_cldfrc, &
                                  pumas_strat_ice_cldfrc, pumas_qsatfac,      &
                                  pumas_naai, pumas_npccn,                    &
                                  pumas_rndst, pumas_nacon,                   &
                                  pumas_snowice_tend_external,                   &
                                  pumas_numsnow_tend_external,                   &
                                  pumas_effi_external, pumas_frzimm,          &
                                  pumas_frzcnt, pumas_frzdep,                 &
! output vars
                                  pumas_qcsinksum_rate1ord,                     &
                                  pumas_airT_tend, pumas_airq_tend,         &
                                  pumas_cldliq_tend, pumas_cldice_tend,     &
                                  pumas_numliq_tend, pumas_numice_tend,     &
                                  pumas_rainliq_tend, pumas_snowice_tend,   &
                                  pumas_numrain_tend, pumas_numsnow_tend,   &
                                  pumas_graupice_tend, pumas_numgraup_tend, &
                                  pumas_effc, pumas_effc_fn,                &
                                  pumas_effi, pumas_sadice,                 &
                                  pumas_sadsnow, pumas_prect,               &
                                  pumas_preci, pumas_prec_evap,             &
                                  pumas_am_evap_st, pumas_prec_prod,        &
                                  pumas_cmeice, pumas_deffi,                &
                                  pumas_pgamrad, pumas_lamcrad,             &
                                  pumas_snowice_in_prec,                        &
                                  pumas_scaled_diam_snow,                       &
                                  pumas_graupice_in_prec,                       &
                                  pumas_numgraup_vol_in_prec,                   &
                                  pumas_scaled_diam_graup,                      &
                                  pumas_lflx, pumas_iflx, pumas_gflx,   &
                                  pumas_rflx, pumas_sflx,                   &
                                  pumas_rainliq_in_prec, pumas_reff_rain,   &
                                  pumas_reff_snow, pumas_reff_grau,         &
                                  pumas_numrain_vol_in_prec,                    &
                                  pumas_numsnow_vol_in_prec,                    &
                                  pumas_refl, pumas_arefl,                  &
                                  pumas_areflz, pumas_frefl,                &
                                  pumas_csrfl, pumas_acsrfl,                &
                                  pumas_fcsrfl, pumas_refl10cm,             &
                                  pumas_reflz10cm, pumas_rercld,            &
                                  pumas_ncai, pumas_ncal,                   &
                                  pumas_rainliq_out, pumas_snowice_out,             &
                                  pumas_numrain_vol, pumas_numsnow_vol,     &
                                  pumas_diam_rain, pumas_diam_snow,         &
                                  pumas_graupice_out, pumas_numgraup_vol,       &
                                  pumas_diam_graup, pumas_freq_graup,       &
                                  pumas_freq_snow, pumas_freq_rain,         &
                                  pumas_frac_ice, pumas_frac_cldliq_tend,   &
                                  pumas_rain_evap, micro_proc_rates_inout,      &
                                  errmsg, errcode)

    !External dependencies:
    use ccpp_kinds,        only: kind_phys
    use micro_pumas_v1,    only: micro_pumas_tend
    use micro_pumas_diags, only: proc_rates_type
    use pumas_kinds,       only: pumas_r8=>kind_r8

    !Subroutine (dummy) input arguments:

    !Host model dimensions/parameters:
    integer,         intent(in) :: micro_ncol         !Number of horizontal microphysics columns (count)
    integer,         intent(in) :: micro_nlev         !Number of microphysics vertical layers (count)
    integer,         intent(in) :: micro_nlevp1       !Number of microphysics vertical interfaces (count)
    integer,         intent(in) :: micro_dust_nbins   !Number of dust size bins

    real(pumas_r8), intent(in) :: pumas_timestep  !Microphysics time step (s)

    !Host model state variables:

    !Microphysics Air temperature (K)
    real(pumas_r8), intent(in) :: pumas_airT(:,:)
    !Microphysics Water vapor mixing ratio wrt moist air and condensed water (kg kg-1)
    real(pumas_r8), intent(in) :: pumas_airq(:,:)
    !Microphysics cloud liquid water mixing ratio wrt moist air and condensed water (kg kg-1)
    real(pumas_r8), intent(in) :: pumas_cldliq(:,:)
    !Microphysics cloud ice mixing ratio wrt moist air and condensed water (kg kg-1)
    real(pumas_r8), intent(in) :: pumas_cldice(:,:)
    !microphysics mass number concentration of cloud liquid water wrt moist air and condensed water (kg-1)
    real(pumas_r8), intent(in) :: pumas_numliq(:,:)
    !microphysics mass number concentration of cloud ice wrt moist air and condensed water (kg-1)
    real(pumas_r8), intent(in) :: pumas_numice(:,:)
    !microphysics rain mixing ratio wrt moist air and condensed water (kg kg-1)
    real(pumas_r8), intent(in) :: pumas_rainliq(:,:)
    !microphysics snow mixing ratio wrt moist air and condensed water (kg kg-1)
    real(pumas_r8), intent(in) :: pumas_snowice(:,:)
    !microphysics mass number concentration of rain wrt moist air and condensed water (kg-1)
    real(pumas_r8), intent(in) :: pumas_numrain(:,:)
    !microphysics mass number concentration of snow wrt moist air and condensed water (kg-1)
    real(pumas_r8), intent(in) :: pumas_numsnow(:,:)
    !microphysics graupel mixing ratio wrt moist air and condensed water (kg kg-1)
    real(pumas_r8), intent(in) :: pumas_graupice(:,:)
    !microphysics mass number concentration of graupel wrt moist air and condensed water (kg-1)
    real(pumas_r8), intent(in) :: pumas_numgraup(:,:)
    !microphysics relative variance of cloud water (1)
    real(pumas_r8), intent(in) :: pumas_relvar(:,:)
    !microphysics accretion enhancement factor (1)
    real(pumas_r8), intent(in) :: pumas_accre_enhan(:,:)
    !microphysics air pressure (Pa)
    real(pumas_r8), intent(in) :: pumas_pmid(:,:)
    !microphysics air pressure thickness (Pa)
    real(pumas_r8), intent(in) :: pumas_pdel(:,:)
    !microphysics air pressure at interfaces (Pa)
    real(pumas_r8), intent(in) :: pumas_pint(:,:)
    !microphysics stratiform cloud area fraction (fraction)
    real(pumas_r8), intent(in) :: pumas_strat_cldfrc(:,:)
    !microphysics stratiform cloud liquid area fraction (fraction)
    real(pumas_r8), intent(in) :: pumas_strat_liq_cldfrc(:,:)
    !microphysics stratiform cloud ice area fraction (fraction)
    real(pumas_r8), intent(in) :: pumas_strat_ice_cldfrc(:,:)
    !microphysics subgrid cloud water saturation scaling factor (1)
    real(pumas_r8), intent(in) :: pumas_qsatfac(:,:)
    !microphysics tendency of activated ice nuclei mass number concentration (kg-1 s-1)
    real(pumas_r8), intent(in) :: pumas_naai(:,:)
    !microphysics tendency of activated cloud condensation nuclei mass number concentration (kg-1 s-1)
    real(pumas_r8), intent(in) :: pumas_npccn(:,:)
    !microphysics dust radii by size bin  (m)
    real(pumas_r8), intent(in) :: pumas_rndst(:,:,:)
    !microphysics dust number concentration by size bin (m-3)
    real(pumas_r8), intent(in) :: pumas_nacon(:,:,:)
    !microphysics tendency of snow mixing ratio wrt moist air and condensed water from external microphysics (kg kg-1 s-1)
    real(pumas_r8), intent(in) :: pumas_snowice_tend_external(:,:)
    !microphysics tendency of mass number concentration of snow wrt moist air and condensed water from external microphysics
    !(kg-1 s-1)
    real(pumas_r8), intent(in) :: pumas_numsnow_tend_external(:,:)
    !microphysics effective radius of stratiform cloud ice particle from external microphysics (m)
    real(pumas_r8), intent(in) :: pumas_effi_external(:,:)
    !microphysics tendency of cloud liquid droplet number concentration due to immersion freezing (cm-3)
    real(pumas_r8), intent(in) :: pumas_frzimm(:,:)
    !microphysics tendency of cloud liquid droplet number concentration due to contact freezing (cm-3)
    real(pumas_r8), intent(in) :: pumas_frzcnt(:,:)
    !microphysics tendency of cloud ice number concentration due to deposition nucleation (cm-3)
    real(pumas_r8), intent(in) :: pumas_frzdep(:,:)

    !Subroutine output arguments:

    !microphysics direct conversion rate of stratiform cloud water to precipitation (s-1)
    real(pumas_r8), intent(out) :: pumas_qcsinksum_rate1ord(:,:)
    !microphysics tendency of dry air enthalpy at constant pressure (J kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_airT_tend(:,:)
    !microphysics tendency of water vapor mixing ratio wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_airq_tend(:,:)
    !microphysics tendency of cloud liquid water mixing ratio wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_cldliq_tend(:,:)
    !microphysics tendency of cloud ice mixing ratio wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_cldice_tend(:,:)
    !microphysics tendency of mass number concentration of cloud liquid water wrt moist air and condensed water (kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_numliq_tend(:,:)
    !microphysics tendency of mass number concentration of cloud ice wrt moist air and condensed water (kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_numice_tend(:,:)
    !microphysics tendency of rain mixing ratio wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_rainliq_tend(:,:)
    !microphysics tendency of snow mixing ratio wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_snowice_tend(:,:)
    !microphysics tendency of mass number concentration of rain wrt moist air and condensed water (kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_numrain_tend(:,:)
    !microphysics tendency of mass number concentration of snow wrt moist air and condensed water (kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_numsnow_tend(:,:)
    !microphysics tendency of graupel mixing ratio wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_graupice_tend(:,:)
    !microphysics tendency of mass number concentration of graupel wrt moist air and condensed water (kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_numgraup_tend(:,:)
    !microphysics effective radius of stratiform cloud liquid water particle (um)
    real(pumas_r8), intent(out) :: pumas_effc(:,:)
    !microphysics effective radius of stratiform cloud liquid water particle assuming droplet number concentration of 1e8 kg-1 (um)
    real(pumas_r8), intent(out) :: pumas_effc_fn(:,:)
    !microphysics effective radius of stratiform cloud ice particle (um)
    real(pumas_r8), intent(out) :: pumas_effi(:,:)
    !microphysics cloud ice surface area density (cm2 cm-3)
    real(pumas_r8), intent(out) :: pumas_sadice(:,:)
    !microphysics snow surface area density (cm2 cm-3)
    real(pumas_r8), intent(out) :: pumas_sadsnow(:,:)
    !microphysics LWE large scale precipitation rate at surface (m s-1)
    real(pumas_r8), intent(out) :: pumas_prect(:)
    !microphysics LWE large scale snowfall rate at surface (m s-1)
    real(pumas_r8), intent(out) :: pumas_preci(:)
    !microphysics precipitation evaporation rate wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_prec_evap(:,:)
    !microphysics precipitation evaporation area (fraction)
    real(pumas_r8), intent(out) :: pumas_am_evap_st(:,:)
    !microphysics precipitation production rate wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_prec_prod(:,:)
    !microphysics condensation minus evaporation rate of in-cloud ice wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_cmeice(:,:)
    !microphysics effective diameter of stratiform cloud ice particles for radiation (um)
    real(pumas_r8), intent(out) :: pumas_deffi(:,:)
    !microphysics cloud particle size distribution shape parameter (1)
    real(pumas_r8), intent(out) :: pumas_pgamrad(:,:)
    !microphysics cloud particle size distribution slope parameter (1)
    real(pumas_r8), intent(out) :: pumas_lamcrad(:,:)
    !microphysics snow mixing ratio wrt moist air and condensed water of new state in precipitating fraction of gridcell (kg kg-1)
    real(pumas_r8), intent(out) :: pumas_snowice_in_prec(:,:)
    !microphysics snow scaled diameter (m)
    real(pumas_r8), intent(out) :: pumas_scaled_diam_snow(:,:)
    !microphysics graupel mixing ratio wrt moist air and condensed water of new state in precipitating fraction of gridcell (kg kg-1)
    real(pumas_r8), intent(out) :: pumas_graupice_in_prec(:,:)
    !microphysics graupel number concentration of new state in precipitating fraction of gridcell (m-3)
    real(pumas_r8), intent(out) :: pumas_numgraup_vol_in_prec(:,:)
    !microphysics graupel scaled diameter (m)
    real(pumas_r8), intent(out) :: pumas_scaled_diam_graup(:,:)
    !microphysics cloud liquid sedimentation flux (kg m-2 s-1)
    real(pumas_r8), intent(out) :: pumas_lflx(:,:)
    !microphysics cloud ice sedimentation flux (kg m-2 s-1)
    real(pumas_r8), intent(out) :: pumas_iflx(:,:)
    !microphysics graupel sedimentation flux (kg m-2 s-1)
    real(pumas_r8), intent(out) :: pumas_gflx(:,:)
    !microphysics rain sedimentation flux (kg m-2 s-1)
    real(pumas_r8), intent(out) :: pumas_rflx(:,:)
    !microphysics snow sedimentation flux (kg m-2 s-1)
    real(pumas_r8), intent(out) :: pumas_sflx(:,:)
    !microphysics rain mixing ratio wrt moist air and condensed water of new state in precipitating fraction of gridcell (kg kg-1)
    real(pumas_r8), intent(out) :: pumas_rainliq_in_prec(:,:)
    !microphysics effective radius of stratiform rain particle (um)
    real(pumas_r8), intent(out) :: pumas_reff_rain(:,:)
    !microphysics effective radius of stratiform snow particle (um)
    real(pumas_r8), intent(out) :: pumas_reff_snow(:,:)
    !microphysics effective radius of stratiform graupel particle (um)
    real(pumas_r8), intent(out) :: pumas_reff_grau(:,:)
    !microphysics rain number concentration of new state in precipitating fraction of gridcell (m-3)
    real(pumas_r8), intent(out) :: pumas_numrain_vol_in_prec(:,:)
    !microphysics snow number concentration of new state in precipitating fraction of gridcell (m-3)
    real(pumas_r8), intent(out) :: pumas_numsnow_vol_in_prec(:,:)
    !microphysics analytic radar reflectivity at 94 GHz in precipitating fraction of gridcell (dBZ)
    real(pumas_r8), intent(out) :: pumas_refl(:,:)
    !microphysics analytic radar reflectivity at 94 GHz (dBZ)
    real(pumas_r8), intent(out) :: pumas_arefl(:,:)
    !microphysics analytic radar reflectivity z factor at 94 GHz (mm6 m-3)
    real(pumas_r8), intent(out) :: pumas_areflz(:,:)
    !microphysics fraction of gridcell with nonzero radar reflectivity (fraction)
    real(pumas_r8), intent(out) :: pumas_frefl(:,:)
    !microphysics analytic radar reflectivity at 94 GHz with CloudSat thresholds in precipitating fraction of gridcell (dBZ)
    real(pumas_r8), intent(out) :: pumas_csrfl(:,:)
    !microphysics analytic radar reflectivity at 94 GHz with CloudSat thresholds (dBZ)
    real(pumas_r8), intent(out) :: pumas_acsrfl(:,:)
    !microphysics fraction of gridcell with nonzero radar reflectivity with CloudSat thresholds (fraction)
    real(pumas_r8), intent(out) :: pumas_fcsrfl(:,:)
    !microphysics analytic radar reflectivity at 10 cm wavelength (dBZ)
    real(pumas_r8), intent(out) :: pumas_refl10cm(:,:)
    !microphysics analytic radar reflectivity z factor at 10 cm wavelength (mm6 m-3)
    real(pumas_r8), intent(out) :: pumas_reflz10cm(:,:)
    !microphysics effective radius of stratiform cloud liquid plus rain particles (m)
    real(pumas_r8), intent(out) :: pumas_rercld(:,:)
    !microphysics available ice nuclei number concentration of new state (m-3)
    real(pumas_r8), intent(out) :: pumas_ncai(:,:)
    !microphysics available cloud condensation nuclei number concentration of new state (m-3)
    real(pumas_r8), intent(out) :: pumas_ncal(:,:)
    !microphysics rain mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(pumas_r8), intent(out) :: pumas_rainliq_out(:,:)
    !microphysics snow mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(pumas_r8), intent(out) :: pumas_snowice_out(:,:)
    !microphysics rain number concentration of new state (m-3)
    real(pumas_r8), intent(out) :: pumas_numrain_vol(:,:)
    !microphysics snow number concentration of new state in precipitating fraction of gridcell (m-3)
    real(pumas_r8), intent(out) :: pumas_numsnow_vol(:,:)
    !microphysics average diameter of stratiform rain particle (m)
    real(pumas_r8), intent(out) :: pumas_diam_rain(:,:)
    !microphysics average diameter of stratiform snow particle (m)
    real(pumas_r8), intent(out) :: pumas_diam_snow(:,:)
    !microphysics graupel mixing ratio wrt moist air and condensed water of new state (kg kg-1)
    real(pumas_r8), intent(out) :: pumas_graupice_out(:,:)
    !microphysics graupel number concentration of new state (m-3)
    real(pumas_r8), intent(out) :: pumas_numgraup_vol(:,:)
    !microphysics average diameter of stratiform graupel particle (m)
    real(pumas_r8), intent(out) :: pumas_diam_graup(:,:)
    !microphysics fraction of gridcell with graupel (fraction)
    real(pumas_r8), intent(out) :: pumas_freq_graup(:,:)
    !microphysics fraction of gridcell with snow (fraction)
    real(pumas_r8), intent(out) :: pumas_freq_snow(:,:)
    !microphysics fraction of gridcell with rain (fraction)
    real(pumas_r8), intent(out) :: pumas_freq_rain(:,:)
    !microphysics fraction of frozen water to total condensed water (fraction)
    real(pumas_r8), intent(out) :: pumas_frac_ice(:,:)
    !microphysics fraction of cloud liquid tendency applied to state (fraction)
    real(pumas_r8), intent(out) :: pumas_frac_cldliq_tend(:,:)
    !microphysics rain evaporation rate wrt moist air and condensed water (kg kg-1 s-1)
    real(pumas_r8), intent(out) :: pumas_rain_evap(:,:)
    !microphysics process rates (none)
    type(proc_rates_type), intent(inout) :: micro_proc_rates_inout

    character(len=512), intent(out) :: errmsg  !PUMAS/CCPP error message (none)
    integer,            intent(out) :: errcode !CCPP error code (1)

    !Local variables:
!    real(pumas_r8) :: micro_timestep
!    real(pumas_r8) :: airT(micro_ncol, micro_nlev)
!    real(pumas_r8) :: airq(micro_ncol, micro_nlev)
!    real(pumas_r8) :: cldliq(micro_ncol, micro_nlev)
!    real(pumas_r8) :: cldice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numliq(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: rainliq(micro_ncol, micro_nlev)
!    real(pumas_r8) :: snowice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numrain(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numsnow(micro_ncol, micro_nlev)
!    real(pumas_r8) :: graupice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numgraup(micro_ncol, micro_nlev)
!    real(pumas_r8) :: relvar(micro_ncol, micro_nlev)
!    real(pumas_r8) :: accre_enhan(micro_ncol, micro_nlev)
!    real(pumas_r8) :: pmid(micro_ncol, micro_nlev)
!    real(pumas_r8) :: pdel(micro_ncol, micro_nlev)
!    real(pumas_r8) :: pint(micro_ncol, micro_nlevp1)
!    real(pumas_r8) :: strat_cldfrc(micro_ncol, micro_nlev)
!    real(pumas_r8) :: strat_liq_cldfrc(micro_ncol, micro_nlev)
!    real(pumas_r8) :: strat_ice_cldfrc(micro_ncol, micro_nlev)
!    real(pumas_r8) :: qsatfac(micro_ncol, micro_nlev)
!    real(pumas_r8) :: naai(micro_ncol, micro_nlev)
!    real(pumas_r8) :: npccn(micro_ncol, micro_nlev)
!    real(pumas_r8) :: rndst(micro_ncol, micro_nlev, micro_dust_nbins)
!    real(pumas_r8) :: nacon(micro_ncol, micro_nlev, micro_dust_nbins)
!    real(pumas_r8) :: snowice_tend_external(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numsnow_tend_external(micro_ncol, micro_nlev)
!    real(pumas_r8) :: effi_external(micro_ncol, micro_nlev)
!    real(pumas_r8) :: frzimm(micro_ncol, micro_nlev)
!    real(pumas_r8) :: frzcnt(micro_ncol, micro_nlev)
!    real(pumas_r8) :: frzdep(micro_ncol, micro_nlev)
!    real(pumas_r8) :: qcsinksum_rate1ord(micro_ncol, micro_nlev)
!    real(pumas_r8) :: airT_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: airq_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: cldliq_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: cldice_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numliq_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numice_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: rainliq_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: snowice_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numrain_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numsnow_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: graupice_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numgraup_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: effc(micro_ncol, micro_nlev)
!    real(pumas_r8) :: effc_fn(micro_ncol, micro_nlev)
!    real(pumas_r8) :: effi(micro_ncol, micro_nlev)
!    real(pumas_r8) :: sadice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: sadsnow(micro_ncol, micro_nlev)
!    real(pumas_r8) :: prect(micro_ncol)
!    real(pumas_r8) :: preci(micro_ncol)
!    real(pumas_r8) :: prec_evap(micro_ncol, micro_nlev)
!    real(pumas_r8) :: am_evap_st(micro_ncol, micro_nlev)
!    real(pumas_r8) :: prec_prod(micro_ncol, micro_nlev)
!    real(pumas_r8) :: cmeice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: deffi(micro_ncol, micro_nlev)
!    real(pumas_r8) :: pgamrad(micro_ncol, micro_nlev)
!    real(pumas_r8) :: lamcrad(micro_ncol, micro_nlev)
!    real(pumas_r8) :: snowice_in_prec_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: scaled_diam_snow_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: graupice_in_prec_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numgraup_vol_in_prec_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: scaled_diam_graup_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: lflx(micro_ncol, micro_nlevp1)
!    real(pumas_r8) :: iflx(micro_ncol, micro_nlevp1)
!    real(pumas_r8) :: gflx(micro_ncol, micro_nlevp1)
!    real(pumas_r8) :: rflx(micro_ncol, micro_nlevp1)
!    real(pumas_r8) :: sflx(micro_ncol, micro_nlevp1)
!    real(pumas_r8) :: rainliq_in_prec_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: reff_rain(micro_ncol, micro_nlev)
!    real(pumas_r8) :: reff_snow(micro_ncol, micro_nlev)
!    real(pumas_r8) :: reff_grau(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numrain_vol_in_prec_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numsnow_vol_in_prec_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: refl(micro_ncol, micro_nlev)
!    real(pumas_r8) :: arefl(micro_ncol, micro_nlev)
!    real(pumas_r8) :: areflz(micro_ncol, micro_nlev)
!    real(pumas_r8) :: frefl(micro_ncol, micro_nlev)
!    real(pumas_r8) :: csrfl(micro_ncol, micro_nlev)
!    real(pumas_r8) :: acsrfl(micro_ncol, micro_nlev)
!    real(pumas_r8) :: fcsrfl(micro_ncol, micro_nlev)
!    real(pumas_r8) :: refl10cm(micro_ncol, micro_nlev)
!    real(pumas_r8) :: reflz10cm(micro_ncol, micro_nlev)
!    real(pumas_r8) :: rercld(micro_ncol, micro_nlev)
!    real(pumas_r8) :: ncai(micro_ncol, micro_nlev)
!    real(pumas_r8) :: ncal(micro_ncol, micro_nlev)
!    real(pumas_r8) :: rainliq_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: snowice_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numrain_vol_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numsnow_vol_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: diam_rain_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: diam_snow_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: graupice_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: numgraup_vol_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: diam_graup_out(micro_ncol, micro_nlev)
!    real(pumas_r8) :: freq_graup(micro_ncol, micro_nlev)
!    real(pumas_r8) :: freq_snow(micro_ncol, micro_nlev)
!    real(pumas_r8) :: freq_rain(micro_ncol, micro_nlev)
!    real(pumas_r8) :: frac_ice(micro_ncol, micro_nlev)
!    real(pumas_r8) :: frac_cldliq_tend(micro_ncol, micro_nlev)
!    real(pumas_r8) :: micro_rain_evap(micro_ncol, micro_nlev)

    !Local PUMAS error message
    character(len=128) :: pumas_errstring

    !Initialize error message and error code:
    errmsg  = ''
    errcode = 0

    !Convert all CCPP input real variables to PUMAS precision:
!    micro_timestep        = real(micro_timestep_in, pumas_r8)
!    airT                  = real(micro_airT_in, pumas_r8)
!    airq                  = real(micro_airq_in, pumas_r8)
!    cldliq                = real(micro_cldliq_in, pumas_r8)
!    cldice                = real(micro_cldice_in, pumas_r8)
!    numliq                = real(micro_numliq_in, pumas_r8)
!    numice                = real(micro_numice_in, pumas_r8)
!    rainliq               = real(micro_rainliq_in, pumas_r8)
!    snowice               = real(micro_snowice_in, pumas_r8)
!    numrain               = real(micro_numrain_in, pumas_r8)
!    numsnow               = real(micro_numsnow_in, pumas_r8)
!    graupice              = real(micro_graupice_in, pumas_r8)
!    numgraup              = real(micro_numgraup_in, pumas_r8)
!    relvar                = real(micro_relvar_in, pumas_r8)
!    accre_enhan           = real(micro_accre_enhan_in, pumas_r8)
!    pmid                  = real(micro_pmid_in, pumas_r8)
!    pdel                  = real(micro_pdel_in, pumas_r8)
!    pint                  = real(micro_pint_in, pumas_r8)
!    strat_cldfrc          = real(micro_strat_cldfrc_in, pumas_r8)
!    strat_liq_cldfrc      = real(micro_strat_liq_cldfrc_in, pumas_r8)
!    strat_ice_cldfrc      = real(micro_strat_ice_cldfrc_in, pumas_r8)
!    qsatfac               = real(micro_qsatfac_in, pumas_r8)
!    naai                  = real(micro_naai_in, pumas_r8)
!    npccn                 = real(micro_npccn_in, pumas_r8)
!    rndst                 = real(micro_rndst_in, pumas_r8)
!    nacon                 = real(micro_nacon_in, pumas_r8)
!    snowice_tend_external = real(micro_snowice_tend_external_in, pumas_r8)
!    numsnow_tend_external = real(micro_numsnow_tend_external_in, pumas_r8)
!    effi_external         = real(micro_effi_external_in, pumas_r8)
!    frzimm                = real(micro_frzimm_in, pumas_r8)
!    frzcnt                = real(micro_frzcnt_in, pumas_r8)
!    frzdep                = real(micro_frzdep_in, pumas_r8)

    !Call main PUMAS run routine:
    !---------------------------

    call micro_pumas_tend( &
        micro_ncol,             micro_nlev,     pumas_timestep,      &
        pumas_airT,                   pumas_airq,                                &
        pumas_cldliq,                 pumas_cldice,                              &
        pumas_numliq,                 pumas_numice,                              &
        pumas_rainliq,                pumas_snowice,                             &
        pumas_numrain,                pumas_numsnow,                             &
        pumas_graupice,               pumas_numgraup,                            &
        pumas_relvar,                 pumas_accre_enhan,                   &
        pumas_pmid,                   pumas_pdel, pumas_pint,                          &
        pumas_strat_cldfrc,           pumas_strat_liq_cldfrc,                    &
        pumas_strat_ice_cldfrc,       pumas_qsatfac,                             &
        pumas_qcsinksum_rate1ord,                                          &
        pumas_naai,                   pumas_npccn,                               &
        pumas_rndst,                  pumas_nacon,                               &
        pumas_airT_tend,              pumas_airq_tend,                           &
        pumas_cldliq_tend,            pumas_cldice_tend,                         &
        pumas_numliq_tend,            pumas_numice_tend,                         &
        pumas_rainliq_tend,           pumas_snowice_tend,                        &
        pumas_numrain_tend,           pumas_numsnow_tend,                        &
        pumas_graupice_tend,          pumas_numgraup_tend,                       &
        pumas_effc,                   pumas_effc_fn,        pumas_effi,                &
        pumas_sadice,                 pumas_sadsnow,                             &
        pumas_prect,                  pumas_preci,                               &
        pumas_prec_evap,              pumas_am_evap_st,                          &
        pumas_prec_prod,                                                   &
        pumas_cmeice,                 pumas_deffi,                               &
        pumas_pgamrad,                pumas_lamcrad,                             &
        pumas_snowice_in_prec,    pumas_scaled_diam_snow,                &
        pumas_graupice_in_prec,   pumas_numgraup_vol_in_prec,            &
        pumas_scaled_diam_graup,                                       &
        pumas_lflx,                   pumas_iflx,                                &
        pumas_gflx,                                                        &
        pumas_rflx,                   pumas_sflx,           pumas_rainliq_in_prec, &
        pumas_reff_rain,              pumas_reff_snow,      pumas_reff_grau,           &
        pumas_numrain_vol_in_prec,    pumas_numsnow_vol_in_prec,         &
        pumas_refl,                   pumas_arefl,          pumas_areflz,              &
        pumas_frefl,                  pumas_csrfl,          pumas_acsrfl,              &
        pumas_fcsrfl,   pumas_refl10cm,     pumas_reflz10cm,      pumas_rercld,              &
        pumas_ncai,                   pumas_ncal,                                &
        pumas_rainliq_out,            pumas_snowice_out,                         &
        pumas_numrain_vol,        pumas_numsnow_vol,                     &
        pumas_diam_rain,          pumas_diam_snow,                       &
        pumas_graupice_out,           pumas_numgraup_vol, pumas_diam_graup,    &
        pumas_freq_graup,             pumas_freq_snow,        pumas_freq_rain,         &
        pumas_frac_ice,               pumas_frac_cldliq_tend,                    &
        micro_proc_rates_inout, pumas_errstring,                     &
        pumas_snowice_tend_external,  pumas_numsnow_tend_external,               &
        pumas_effi_external,          pumas_rain_evap,                     &
        pumas_frzimm,                 pumas_frzcnt,           pumas_frzdep           )

     !---------------------------

     !Convert all PUMAS output real variables to CCPP precision:
!     micro_qcsinksum_rate1ord_out   = real(qcsinksum_rate1ord, kind_phys)
!     micro_airT_tend_out            = real(airT_tend, kind_phys)
!     micro_airq_tend_out            = real(airq_tend, kind_phys)
!     micro_cldliq_tend_out          = real(cldliq_tend, kind_phys)
!     micro_cldice_tend_out          = real(cldice_tend, kind_phys)
!     micro_numliq_tend_out          = real(numliq_tend, kind_phys)
!     micro_numice_tend_out          = real(numice_tend, kind_phys)
!     micro_rainliq_tend_out         = real(rainliq_tend, kind_phys)
!     micro_snowice_tend_out         = real(snowice_tend, kind_phys)
!     micro_numrain_tend_out         = real(numrain_tend, kind_phys)
!     micro_numsnow_tend_out         = real(numsnow_tend, kind_phys)
!     micro_graupice_tend_out        = real(graupice_tend, kind_phys)
!     micro_numgraup_tend_out        = real(numgraup_tend, kind_phys)
!     micro_effc_out                 = real(effc, kind_phys)
!     micro_effc_fn_out              = real(effc_fn, kind_phys)
!     micro_effi_out                 = real(effi, kind_phys)
!     micro_sadice_out               = real(sadice, kind_phys)
!     micro_sadsnow_out              = real(sadsnow, kind_phys)
!     micro_prect_out                = real(prect, kind_phys)
!     micro_preci_out                = real(preci, kind_phys)
!     micro_prec_evap_out            = real(prec_evap, kind_phys)
!     micro_am_evap_st_out           = real(am_evap_st, kind_phys)
!     micro_prec_prod_out            = real(prec_prod, kind_phys)
!     micro_cmeice_out               = real(cmeice, kind_phys)
!     micro_deffi_out                = real(deffi, kind_phys)
!     micro_pgamrad_out              = real(pgamrad, kind_phys)
!     micro_lamcrad_out              = real(lamcrad, kind_phys)
!     micro_snowice_in_prec_out      = real(snowice_in_prec_out, kind_phys)
!     micro_scaled_diam_snow_out     = real(scaled_diam_snow_out, kind_phys)
!     micro_graupice_in_prec_out     = real(graupice_in_prec_out, kind_phys)
!     micro_numgraup_vol_in_prec_out = real(numgraup_vol_in_prec_out, kind_phys)
!     micro_scaled_diam_graup_out    = real(scaled_diam_graup_out, kind_phys)
!     micro_lflx_out                 = real(lflx, kind_phys)
!     micro_iflx_out                 = real(iflx, kind_phys)
!     micro_gflx_out                 = real(gflx, kind_phys)
!     micro_rflx_out                 = real(rflx, kind_phys)
!     micro_sflx_out                 = real(sflx, kind_phys)
!     micro_rainliq_in_prec_out      = real(rainliq_in_prec_out, kind_phys)
!     micro_reff_rain_out            = real(reff_rain, kind_phys)
!     micro_reff_snow_out            = real(reff_snow, kind_phys)
!     micro_reff_grau_out            = real(reff_grau, kind_phys)
!     micro_numrain_vol_in_prec_out  = real(numrain_vol_in_prec_out, kind_phys)
!     micro_numsnow_vol_in_prec_out  = real(numsnow_vol_in_prec_out, kind_phys)
!     micro_refl_out                 = real(refl, kind_phys)
!     micro_arefl_out                = real(arefl, kind_phys)
!     micro_areflz_out               = real(areflz, kind_phys)
!     micro_frefl_out                = real(frefl, kind_phys)
!     micro_csrfl_out                = real(csrfl, kind_phys)
!     micro_acsrfl_out               = real(acsrfl, kind_phys)
!     micro_fcsrfl_out               = real(fcsrfl, kind_phys)
!     micro_refl10cm_out             = real(refl10cm, kind_phys)
!     micro_reflz10cm_out            = real(reflz10cm, kind_phys)
!     micro_rercld_out               = real(rercld, kind_phys)
!     micro_ncai_out                 = real(ncai, kind_phys)
!     micro_ncal_out                 = real(ncal, kind_phys)
!     micro_rainliq_out              = real(rainliq_out, kind_phys)
!     micro_snowice_out              = real(snowice_out, kind_phys)
!     micro_numrain_vol_out          = real(numrain_vol_out, kind_phys)
!     micro_numsnow_vol_out          = real(numsnow_vol_out, kind_phys)
!     micro_diam_rain_out            = real(diam_rain_out, kind_phys)
!     micro_diam_snow_out            = real(diam_snow_out, kind_phys)
!     micro_graupice_out             = real(graupice_out, kind_phys)
!     micro_numgraup_vol_out         = real(numgraup_vol_out, kind_phys)
!     micro_diam_graup_out           = real(diam_graup_out, kind_phys)
!     micro_freq_graup_out           = real(freq_graup, kind_phys)
!     micro_freq_snow_out            = real(freq_snow, kind_phys)
!     micro_freq_rain_out            = real(freq_rain, kind_phys)
!     micro_frac_ice_out             = real(frac_ice, kind_phys)
!     micro_frac_cldliq_tend_out     = real(frac_cldliq_tend, kind_phys)
!     micro_rain_evap_out            = real(micro_rain_evap, kind_phys)

    !Set error code to non-zero value if PUMAS returns an error message:
    if (trim(errmsg) /= "") then
      errcode = 1
      errmsg  = trim(pumas_errstring)
    end if

  end subroutine micro_pumas_ccpp_run

end module micro_pumas_ccpp
