unit IBX.Register;

interface

  procedure Register;

implementation

uses
  System.SysUtils,
  System.Classes,
  IBX.IBSQL,
  IBX.IBSQLMonitor,
  IBX.IBStoredProc,
  IBX.IBSubscription,
  IBX.IBTable,
  IBX.IBUpdateSQL,
  IBX.IBUtils,
  IBX.IBVisualConst,
  IBX.IBXConst,
  IBX.IBXMLHeader,
  IBX.IB,
  IBX.IBBatchUpdate,
  IBX.IBBlob,
  IBX.IBConnectionBroker,
  IBX.IBCSMonitor,
  IBX.IBCustomDataSet,
  IBX.IBDatabase,
  IBX.IBDatabaseInfo,
  IBX.IBDatabaseINI,
  IBX.IBErrorCodes,
  IBX.IBEvents,
  IBX.IBExternals,
  IBX.IBExtraction,
  IBX.IBFilterDialog,
  IBX.IBFilterSummary,
  IBX.IBHeader,
  IBX.IBInstall,
  IBX.IBInstallHeader,
  IBX.IBIntf,
  IBX.IBQuery,
  IBX.IBScript,
  IBX.IBServices;

procedure Register;
begin

  // componentes
  RegisterComponents('Interbase', [
    TIBEvents,
    TIBBatchUpdate,
    TIBConnectionBroker,
    TIBPooledConnection,
    TIBMonitorClient,
    TIBMonitorServer,
    TIBDataBase,
    TIBTransaction,
    TIBDatabaseINI,
    TIBSQL,TIBBatch,
    TIBStoredProc,
    TIBSQLMonitor,
    TIBTable,
    TIBUpdateSQL,
    TIBTimer,
    TIBExtraction,
    TIBQuery,
    TIBSQLParser,
    TIBScript,
    TIBBackupService,
    TIBRestoreService,
    TIBValidationService,
    TIBSecurityService,
    TIBStatisticalService]);

end;

end.
