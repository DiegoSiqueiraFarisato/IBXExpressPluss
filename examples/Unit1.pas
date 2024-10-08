unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, IBX.IBCustomDataSet, Vcl.Grids,
  Vcl.DBGrids, IBX.IBTable, IBX.IBDatabase;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    IBTable1: TIBTable;
    DBGrid1: TDBGrid;
    IBTable1NUM_PED: TIBStringField;
    IBTable1NUM_NFI: TIBStringField;
    IBTable1NUM_NFI2: TIBStringField;
    IBTable1NUM_PAR: TIBStringField;
    IBTable1COD_CLI: TIntegerField;
    IBTable1CLIENTE: TIBStringField;
    IBTable1CLIE_FANT: TIBStringField;
    IBTable1COD_VEI: TIBStringField;
    IBTable1VEND_GUER: TIBStringField;
    IBTable1COD_REP: TIBStringField;
    IBTable1REPR_GUER: TIBStringField;
    IBTable1SETOR: TIBStringField;
    IBTable1COND_PAGA: TIBStringField;
    IBTable1CGC: TIBStringField;
    IBTable1INSC_ESTA: TIBStringField;
    IBTable1END_FAT: TIBStringField;
    IBTable1BAIRRO_FAT: TIBStringField;
    IBTable1MUNICI_FAT: TIBStringField;
    IBTable1ESTADO_FAT: TIBStringField;
    IBTable1CEP_FAT: TIBStringField;
    IBTable1EMAIL_FAT: TIBStringField;
    IBTable1FAX_FAT: TIBStringField;
    IBTable1END_COB: TIBStringField;
    IBTable1BAIRRO_COB: TIBStringField;
    IBTable1MUNICI_COB: TIBStringField;
    IBTable1ESTADO_COB: TIBStringField;
    IBTable1CEP_COB: TIBStringField;
    IBTable1EMAIL_COB: TIBStringField;
    IBTable1FAX_COB: TIBStringField;
    IBTable1END_COM: TIBStringField;
    IBTable1BAIRRO_COM: TIBStringField;
    IBTable1MUNICI_COM: TIBStringField;
    IBTable1ESTADO_COM: TIBStringField;
    IBTable1CEP_COM: TIBStringField;
    IBTable1EMAIL_COM: TIBStringField;
    IBTable1FAX_COM: TIBStringField;
    IBTable1END_ENT: TIBStringField;
    IBTable1BAIRRO_ENT: TIBStringField;
    IBTable1MUNICI_ENT: TIBStringField;
    IBTable1ESTADO_ENT: TIBStringField;
    IBTable1CEP_ENT: TIBStringField;
    IBTable1EMAIL_ENT: TIBStringField;
    IBTable1FAX_ENT: TIBStringField;
    IBTable1OBS_ENT: TWideMemoField;
    IBTable1OBS_FAT: TWideMemoField;
    IBTable1OBS_COB: TWideMemoField;
    IBTable1OBS_COM: TWideMemoField;
    IBTable1OBS_CLI: TWideMemoField;
    IBTable1TIPO: TIBStringField;
    IBTable1ALTERA: TIBStringField;
    IBTable1COD_NAT: TIBStringField;
    IBTable1DESC_NATU: TIBStringField;
    IBTable1JUROS: TIBBCDField;
    IBTable1COMPL_FAT: TIBStringField;
    IBTable1COMPL_COB: TIBStringField;
    IBTable1COMPL_COM: TIBStringField;
    IBTable1COMPL_ENT: TIBStringField;
    IBTable1TOTA_ITEM: TIBBCDField;
    IBTable1VALO_TOTA: TIBBCDField;
    IBTable1OBS_PAG: TWideMemoField;
    IBTable1OBS_GER: TWideMemoField;
    IBTable1ESTOQUE: TIBStringField;
    IBTable1OBS_NF: TWideMemoField;
    IBTable1MARCA: TIBStringField;
    IBTable1NUMERO: TIBStringField;
    IBTable1QUANT: TIBBCDField;
    IBTable1ESPECIE: TIBStringField;
    IBTable1PESO_BRUT: TIBBCDField;
    IBTable1PESO_LIQU: TIBBCDField;
    IBTable1COD_TRN: TIntegerField;
    IBTable1TIPO_FCI: TIBStringField;
    IBTable1BASE_CALC: TIBBCDField;
    IBTable1TOT_VEN: TIBBCDField;
    IBTable1TOT_SER: TIBBCDField;
    IBTable1TOT_ICM: TIBBCDField;
    IBTable1TOT_IPI: TIBBCDField;
    IBTable1TOT_ISS: TIBBCDField;
    IBTable1TIPO_FRETE: TSmallintField;
    IBTable1DATA_NOFI: TDateField;
    IBTable1DATA_VENDA: TDateField;
    IBTable1DATA_INI: TDateField;
    IBTable1DATA_ENTR: TDateField;
    IBTable1DATA_PEDI: TDateField;
    IBTable1SITUACAO: TIBStringField;
    IBTable1DOLAR: TIBBCDField;
    IBTable1FONE1_FAT: TIBStringField;
    IBTable1FONE2_FAT: TIBStringField;
    IBTable1FONE1_COB: TIBStringField;
    IBTable1FONE2_COB: TIBStringField;
    IBTable1FONE1_COM: TIBStringField;
    IBTable1FONE2_COM: TIBStringField;
    IBTable1FONE1_ENT: TIBStringField;
    IBTable1FONE2_ENT: TIBStringField;
    IBTable1NUM_PCL: TIBStringField;
    IBTable1CONTAT_FAT: TIBStringField;
    IBTable1CONTAT_COB: TIBStringField;
    IBTable1CONTAT_COM: TIBStringField;
    IBTable1CONTAT_ENT: TIBStringField;
    IBTable1OBS_PED: TWideMemoField;
    IBTable1ESPECIAL: TIBStringField;
    IBTable1FRETE: TIBBCDField;
    IBTable1SEGURO: TIBBCDField;
    IBTable1OUTROS: TIBBCDField;
    IBTable1MOTIVO: TIBStringField;
    IBTable1IRRF: TIBBCDField;
    IBTable1PERC_ISS: TIBBCDField;
    IBTable1TIPO_CLIE: TIBStringField;
    IBTable1CAR_COR: TIBStringField;
    IBTable1DESC_ISS: TIBStringField;
    IBTable1PAIS_FAT: TIBStringField;
    IBTable1NUM_FAT: TIBStringField;
    IBTable1NUM_COB: TIBStringField;
    IBTable1NUM_COM: TIBStringField;
    IBTable1NUM_ENT: TIBStringField;
    IBTable1DESCONTO: TIBBCDField;
    IBTable1LOCAL_EMBARQUE: TIBStringField;
    IBTable1TOT_II: TIBBCDField;
    IBTable1TX_SISCOMEX_TOTAL: TIBBCDField;
    IBTable1COD_EMP: TIntegerField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
