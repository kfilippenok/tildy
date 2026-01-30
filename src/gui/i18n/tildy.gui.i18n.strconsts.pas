unit Tildy.GUI.i18n.StrConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SCancel = 'Cancel';
  SApply = 'Apply';
  SAdd = 'Add';
  SZoom = 'Zoom';
  SLat = 'Latitude';
  SLon = 'Longitude';

  { Menu }
  SMenuLanguage = 'Language';
  SHMenuLanguage = 'Change language';

  { Areas }

  SAreasAdd = 'Add';
  SAreasDelete = 'Delete';
  SAreasRename = 'Rename';
  SAreasExport = 'Export';
  SAreasImport = 'Import';

  SHAreasAdd = 'Add new area';
  SHAreasDelete = 'Delete selected area';
  SHAreasRename = 'Rename selected area';
  SHAreasExport = 'Export areas to file';
  SHAreasImport = 'Import areas from file';

  SAreasErrImport = 'Error import areas from file: %s';
  SAreasErrExport = 'Error export areas to file: %s';

  { Area Editor Plugin }

  SPluginAreaEditorName = 'Area Editor';
  SPluginAreaEditorDescription = 'Plugin for select and edit areas.';
  SPluginAreaEditorAreaName = 'Area';


implementation

end.

