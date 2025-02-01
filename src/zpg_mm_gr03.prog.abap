**---------------------------------------------------------------------*
*& Program name: ZPG_MM_GR03
*& Created by:   TrucTT, PhongTT
*& Created date:
*& Report name: Material Management
*& Transport request:
*& Package: S40K900908
**---------------------------------------------------------------------*
REPORT zpg_mm_gr03 MESSAGE-ID zmgr03.

INCLUDE zinzmm01_top.
INCLUDE zinzmm01_s01.
INCLUDE zinzmm01_d01.
INCLUDE zinzmm01_p01.
INCLUDE zinzmm01_f01.
INCLUDE zinzmm01_f02.
INCLUDE zinzmm01_f03.
INCLUDE zinzmm01_pai_9000.
INCLUDE zinzmm01_pbo_9000.


*----------------------------------------------------------------------*
*                   I N I T I A L I Z A T I O N
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM func_1000.
  PERFORM tabstrip.


*----------------------------------------------------------------------*
*              A T   S E L E C T I O N  - S C R E E N
*----------------------------------------------------------------------*
* Modify selection screen
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

** Check material number
 AT SELECTION-SCREEN ON smatnr.
  PERFORM validate_material.

* Check material type
AT SELECTION-SCREEN ON smtart.
  PERFORM validate_mat_type.

* Check plant
AT SELECTION-SCREEN ON swerks.
  PERFORM validate_plant.

* Check storage location
AT SELECTION-SCREEN ON slgort.
  PERFORM validate_sloc.

* Check industry sector
AT SELECTION-SCREEN ON smbrsh.
  PERFORM validate_ind_sector.

* Search help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pfile.
  lcl_excel=>f4_file_name( CHANGING
                             ch_filepath = pfile ).

AT SELECTION-SCREEN.
  PERFORM handle_func_1000.
  PERFORM handle_tabstrip.
  PERFORM check_authority.

*----------------------------------------------------------------------*
*              S T A R T  -  O F  -  S E L E C T I O N
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM check_obligatory.
  CASE mytab-dynnr.
    WHEN 9010.  "Display flow
      PERFORM display_material.
    WHEN 9020.  "Upload flow
      PERFORM upload_material.
      PERFORM display_upload.
  ENDCASE.
