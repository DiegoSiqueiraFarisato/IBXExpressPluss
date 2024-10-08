object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 504
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    704
    504)
  TextHeight = 15
  object DBGrid1: TDBGrid
    Left = 224
    Top = 64
    Width = 472
    Height = 432
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = IBTable1
    Left = 72
    Top = 80
  end
  object IBDatabase1: TIBDatabase
    Connected = True
    DatabaseName = '192.168.139.131:F:\db\DADOS_ELETROFUSI\SIA.GDB'
    Params.Strings = (
      'password=masterkey'
      'user_name=sysdba')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    ServerType = 'IBServer'
    Left = 72
    Top = 152
  end
  object IBTransaction1: TIBTransaction
    Active = True
    Left = 72
    Top = 224
  end
  object IBTable1: TIBTable
    Database = IBDatabase1
    Transaction = IBTransaction1
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    FieldDefs = <
      item
        Name = 'NUM_PED'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'NUM_NFI'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'NUM_NFI2'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'NUM_PAR'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 2
      end
      item
        Name = 'COD_CLI'
        DataType = ftInteger
      end
      item
        Name = 'CLIENTE'
        DataType = ftWideString
        Size = 80
      end
      item
        Name = 'CLIE_FANT'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'COD_VEI'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 4
      end
      item
        Name = 'VEND_GUER'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COD_REP'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 4
      end
      item
        Name = 'REPR_GUER'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'SETOR'
        DataType = ftWideString
        Size = 5
      end
      item
        Name = 'COND_PAGA'
        DataType = ftWideString
        Size = 11
      end
      item
        Name = 'CGC'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'INSC_ESTA'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'END_FAT'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'BAIRRO_FAT'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'MUNICI_FAT'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'ESTADO_FAT'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 2
      end
      item
        Name = 'CEP_FAT'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 9
      end
      item
        Name = 'EMAIL_FAT'
        DataType = ftWideString
        Size = 40
      end
      item
        Name = 'FAX_FAT'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'END_COB'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'BAIRRO_COB'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'MUNICI_COB'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'ESTADO_COB'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 2
      end
      item
        Name = 'CEP_COB'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 9
      end
      item
        Name = 'EMAIL_COB'
        DataType = ftWideString
        Size = 40
      end
      item
        Name = 'FAX_COB'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'END_COM'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'BAIRRO_COM'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'MUNICI_COM'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'ESTADO_COM'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 2
      end
      item
        Name = 'CEP_COM'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 9
      end
      item
        Name = 'EMAIL_COM'
        DataType = ftWideString
        Size = 40
      end
      item
        Name = 'FAX_COM'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'END_ENT'
        DataType = ftWideString
        Size = 50
      end
      item
        Name = 'BAIRRO_ENT'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'MUNICI_ENT'
        DataType = ftWideString
        Size = 25
      end
      item
        Name = 'ESTADO_ENT'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 2
      end
      item
        Name = 'CEP_ENT'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 9
      end
      item
        Name = 'EMAIL_ENT'
        DataType = ftWideString
        Size = 40
      end
      item
        Name = 'FAX_ENT'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'OBS_ENT'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'OBS_FAT'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'OBS_COB'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'OBS_COM'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'OBS_CLI'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'TIPO'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'ALTERA'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'COD_NAT'
        DataType = ftWideString
        Size = 4
      end
      item
        Name = 'DESC_NATU'
        DataType = ftWideString
        Size = 60
      end
      item
        Name = 'JUROS'
        DataType = ftBCD
        Precision = 5
        Size = 2
      end
      item
        Name = 'COMPL_FAT'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COMPL_COB'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COMPL_COM'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COMPL_ENT'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'TOTA_ITEM'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'VALO_TOTA'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'OBS_PAG'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'OBS_GER'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'ESTOQUE'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'OBS_NF'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'MARCA'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'NUMERO'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'QUANT'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'ESPECIE'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'PESO_BRUT'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'PESO_LIQU'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'COD_TRN'
        DataType = ftInteger
      end
      item
        Name = 'TIPO_FCI'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'BASE_CALC'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TOT_VEN'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TOT_SER'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TOT_ICM'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TOT_IPI'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TOT_ISS'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TIPO_FRETE'
        DataType = ftSmallint
      end
      item
        Name = 'DATA_NOFI'
        DataType = ftDate
      end
      item
        Name = 'DATA_VENDA'
        DataType = ftDate
      end
      item
        Name = 'DATA_INI'
        DataType = ftDate
      end
      item
        Name = 'DATA_ENTR'
        DataType = ftDate
      end
      item
        Name = 'DATA_PEDI'
        DataType = ftDate
      end
      item
        Name = 'SITUACAO'
        DataType = ftWideString
        Size = 4
      end
      item
        Name = 'DOLAR'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'FONE1_FAT'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE2_FAT'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE1_COB'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE2_COB'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE1_COM'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE2_COM'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE1_ENT'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'FONE2_ENT'
        DataType = ftWideString
        Size = 14
      end
      item
        Name = 'NUM_PCL'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'CONTAT_FAT'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'CONTAT_COB'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'CONTAT_COM'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'CONTAT_ENT'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'OBS_PED'
        DataType = ftWideMemo
        Size = 8
      end
      item
        Name = 'ESPECIAL'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'FRETE'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'SEGURO'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'OUTROS'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'MOTIVO'
        DataType = ftWideString
        Size = 100
      end
      item
        Name = 'IRRF'
        DataType = ftBCD
        Precision = 8
        Size = 2
      end
      item
        Name = 'PERC_ISS'
        DataType = ftBCD
        Precision = 4
        Size = 2
      end
      item
        Name = 'TIPO_CLIE'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'CAR_COR'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'DESC_ISS'
        Attributes = [faFixed]
        DataType = ftWideString
        Size = 1
      end
      item
        Name = 'PAIS_FAT'
        DataType = ftWideString
        Size = 80
      end
      item
        Name = 'NUM_FAT'
        DataType = ftWideString
        Size = 5
      end
      item
        Name = 'NUM_COB'
        DataType = ftWideString
        Size = 5
      end
      item
        Name = 'NUM_COM'
        DataType = ftWideString
        Size = 5
      end
      item
        Name = 'NUM_ENT'
        DataType = ftWideString
        Size = 5
      end
      item
        Name = 'DESCONTO'
        DataType = ftBCD
        Precision = 9
        Size = 2
      end
      item
        Name = 'LOCAL_EMBARQUE'
        DataType = ftWideString
        Size = 60
      end
      item
        Name = 'TOT_II'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'TX_SISCOMEX_TOTAL'
        DataType = ftBCD
        Precision = 18
        Size = 2
      end
      item
        Name = 'COD_EMP'
        DataType = ftInteger
      end>
    IndexDefs = <
      item
        Name = 'IND_CAD_NFV_NUM_NFI'
        Fields = 'NUM_NFI'
      end
      item
        Name = 'IND_CAD_NFV_CLIE_FANT'
        Fields = 'CLIE_FANT'
      end
      item
        Name = 'IND_CAD_NFV_NUM_NFI_DESC'
        DescFields = 'NUM_NFI'
        Fields = 'NUM_NFI'
        Options = [ixDescending]
      end>
    StoreDefs = True
    TableName = 'CAD_NFV'
    UniDirectional = False
    Left = 168
    Top = 96
    object IBTable1NUM_PED: TIBStringField
      FieldName = 'NUM_PED'
      Origin = '"CAD_NFV"."NUM_PED"'
      Size = 7
    end
    object IBTable1NUM_NFI: TIBStringField
      FieldName = 'NUM_NFI'
      Origin = '"CAD_NFV"."NUM_NFI"'
      Size = 7
    end
    object IBTable1NUM_NFI2: TIBStringField
      FieldName = 'NUM_NFI2'
      Origin = '"CAD_NFV"."NUM_NFI2"'
      Size = 7
    end
    object IBTable1NUM_PAR: TIBStringField
      FieldName = 'NUM_PAR'
      Origin = '"CAD_NFV"."NUM_PAR"'
      Size = 2
    end
    object IBTable1COD_CLI: TIntegerField
      FieldName = 'COD_CLI'
      Origin = '"CAD_NFV"."COD_CLI"'
    end
    object IBTable1CLIENTE: TIBStringField
      FieldName = 'CLIENTE'
      Origin = '"CAD_NFV"."CLIENTE"'
      Size = 80
    end
    object IBTable1CLIE_FANT: TIBStringField
      FieldName = 'CLIE_FANT'
      Origin = '"CAD_NFV"."CLIE_FANT"'
      Size = 15
    end
    object IBTable1COD_VEI: TIBStringField
      FieldName = 'COD_VEI'
      Origin = '"CAD_NFV"."COD_VEI"'
      Size = 4
    end
    object IBTable1VEND_GUER: TIBStringField
      FieldName = 'VEND_GUER'
      Origin = '"CAD_NFV"."VEND_GUER"'
      Size = 10
    end
    object IBTable1COD_REP: TIBStringField
      FieldName = 'COD_REP'
      Origin = '"CAD_NFV"."COD_REP"'
      Size = 4
    end
    object IBTable1REPR_GUER: TIBStringField
      FieldName = 'REPR_GUER'
      Origin = '"CAD_NFV"."REPR_GUER"'
      Size = 10
    end
    object IBTable1SETOR: TIBStringField
      FieldName = 'SETOR'
      Origin = '"CAD_NFV"."SETOR"'
      Size = 5
    end
    object IBTable1COND_PAGA: TIBStringField
      FieldName = 'COND_PAGA'
      Origin = '"CAD_NFV"."COND_PAGA"'
      Size = 11
    end
    object IBTable1CGC: TIBStringField
      FieldName = 'CGC'
      Origin = '"CAD_NFV"."CGC"'
    end
    object IBTable1INSC_ESTA: TIBStringField
      FieldName = 'INSC_ESTA'
      Origin = '"CAD_NFV"."INSC_ESTA"'
    end
    object IBTable1END_FAT: TIBStringField
      FieldName = 'END_FAT'
      Origin = '"CAD_NFV"."END_FAT"'
      Size = 50
    end
    object IBTable1BAIRRO_FAT: TIBStringField
      FieldName = 'BAIRRO_FAT'
      Origin = '"CAD_NFV"."BAIRRO_FAT"'
      Size = 25
    end
    object IBTable1MUNICI_FAT: TIBStringField
      FieldName = 'MUNICI_FAT'
      Origin = '"CAD_NFV"."MUNICI_FAT"'
      Size = 25
    end
    object IBTable1ESTADO_FAT: TIBStringField
      FieldName = 'ESTADO_FAT'
      Origin = '"CAD_NFV"."ESTADO_FAT"'
      Size = 2
    end
    object IBTable1CEP_FAT: TIBStringField
      FieldName = 'CEP_FAT'
      Origin = '"CAD_NFV"."CEP_FAT"'
      Size = 9
    end
    object IBTable1EMAIL_FAT: TIBStringField
      FieldName = 'EMAIL_FAT'
      Origin = '"CAD_NFV"."EMAIL_FAT"'
      Size = 40
    end
    object IBTable1FAX_FAT: TIBStringField
      FieldName = 'FAX_FAT'
      Origin = '"CAD_NFV"."FAX_FAT"'
      Size = 15
    end
    object IBTable1END_COB: TIBStringField
      FieldName = 'END_COB'
      Origin = '"CAD_NFV"."END_COB"'
      Size = 50
    end
    object IBTable1BAIRRO_COB: TIBStringField
      FieldName = 'BAIRRO_COB'
      Origin = '"CAD_NFV"."BAIRRO_COB"'
      Size = 25
    end
    object IBTable1MUNICI_COB: TIBStringField
      FieldName = 'MUNICI_COB'
      Origin = '"CAD_NFV"."MUNICI_COB"'
      Size = 25
    end
    object IBTable1ESTADO_COB: TIBStringField
      FieldName = 'ESTADO_COB'
      Origin = '"CAD_NFV"."ESTADO_COB"'
      Size = 2
    end
    object IBTable1CEP_COB: TIBStringField
      FieldName = 'CEP_COB'
      Origin = '"CAD_NFV"."CEP_COB"'
      Size = 9
    end
    object IBTable1EMAIL_COB: TIBStringField
      FieldName = 'EMAIL_COB'
      Origin = '"CAD_NFV"."EMAIL_COB"'
      Size = 40
    end
    object IBTable1FAX_COB: TIBStringField
      FieldName = 'FAX_COB'
      Origin = '"CAD_NFV"."FAX_COB"'
      Size = 15
    end
    object IBTable1END_COM: TIBStringField
      FieldName = 'END_COM'
      Origin = '"CAD_NFV"."END_COM"'
      Size = 50
    end
    object IBTable1BAIRRO_COM: TIBStringField
      FieldName = 'BAIRRO_COM'
      Origin = '"CAD_NFV"."BAIRRO_COM"'
      Size = 25
    end
    object IBTable1MUNICI_COM: TIBStringField
      FieldName = 'MUNICI_COM'
      Origin = '"CAD_NFV"."MUNICI_COM"'
      Size = 25
    end
    object IBTable1ESTADO_COM: TIBStringField
      FieldName = 'ESTADO_COM'
      Origin = '"CAD_NFV"."ESTADO_COM"'
      Size = 2
    end
    object IBTable1CEP_COM: TIBStringField
      FieldName = 'CEP_COM'
      Origin = '"CAD_NFV"."CEP_COM"'
      Size = 9
    end
    object IBTable1EMAIL_COM: TIBStringField
      FieldName = 'EMAIL_COM'
      Origin = '"CAD_NFV"."EMAIL_COM"'
      Size = 40
    end
    object IBTable1FAX_COM: TIBStringField
      FieldName = 'FAX_COM'
      Origin = '"CAD_NFV"."FAX_COM"'
      Size = 15
    end
    object IBTable1END_ENT: TIBStringField
      FieldName = 'END_ENT'
      Origin = '"CAD_NFV"."END_ENT"'
      Size = 50
    end
    object IBTable1BAIRRO_ENT: TIBStringField
      FieldName = 'BAIRRO_ENT'
      Origin = '"CAD_NFV"."BAIRRO_ENT"'
      Size = 25
    end
    object IBTable1MUNICI_ENT: TIBStringField
      FieldName = 'MUNICI_ENT'
      Origin = '"CAD_NFV"."MUNICI_ENT"'
      Size = 25
    end
    object IBTable1ESTADO_ENT: TIBStringField
      FieldName = 'ESTADO_ENT'
      Origin = '"CAD_NFV"."ESTADO_ENT"'
      Size = 2
    end
    object IBTable1CEP_ENT: TIBStringField
      FieldName = 'CEP_ENT'
      Origin = '"CAD_NFV"."CEP_ENT"'
      Size = 9
    end
    object IBTable1EMAIL_ENT: TIBStringField
      FieldName = 'EMAIL_ENT'
      Origin = '"CAD_NFV"."EMAIL_ENT"'
      Size = 40
    end
    object IBTable1FAX_ENT: TIBStringField
      FieldName = 'FAX_ENT'
      Origin = '"CAD_NFV"."FAX_ENT"'
      Size = 15
    end
    object IBTable1OBS_ENT: TWideMemoField
      FieldName = 'OBS_ENT'
      Origin = '"CAD_NFV"."OBS_ENT"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1OBS_FAT: TWideMemoField
      FieldName = 'OBS_FAT'
      Origin = '"CAD_NFV"."OBS_FAT"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1OBS_COB: TWideMemoField
      FieldName = 'OBS_COB'
      Origin = '"CAD_NFV"."OBS_COB"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1OBS_COM: TWideMemoField
      FieldName = 'OBS_COM'
      Origin = '"CAD_NFV"."OBS_COM"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1OBS_CLI: TWideMemoField
      FieldName = 'OBS_CLI'
      Origin = '"CAD_NFV"."OBS_CLI"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1TIPO: TIBStringField
      FieldName = 'TIPO'
      Origin = '"CAD_NFV"."TIPO"'
      Size = 1
    end
    object IBTable1ALTERA: TIBStringField
      FieldName = 'ALTERA'
      Origin = '"CAD_NFV"."ALTERA"'
      Size = 1
    end
    object IBTable1COD_NAT: TIBStringField
      FieldName = 'COD_NAT'
      Origin = '"CAD_NFV"."COD_NAT"'
      Size = 4
    end
    object IBTable1DESC_NATU: TIBStringField
      FieldName = 'DESC_NATU'
      Origin = '"CAD_NFV"."DESC_NATU"'
      Size = 60
    end
    object IBTable1JUROS: TIBBCDField
      FieldName = 'JUROS'
      Origin = '"CAD_NFV"."JUROS"'
      Precision = 9
      Size = 2
    end
    object IBTable1COMPL_FAT: TIBStringField
      FieldName = 'COMPL_FAT'
      Origin = '"CAD_NFV"."COMPL_FAT"'
      Size = 10
    end
    object IBTable1COMPL_COB: TIBStringField
      FieldName = 'COMPL_COB'
      Origin = '"CAD_NFV"."COMPL_COB"'
      Size = 10
    end
    object IBTable1COMPL_COM: TIBStringField
      FieldName = 'COMPL_COM'
      Origin = '"CAD_NFV"."COMPL_COM"'
      Size = 10
    end
    object IBTable1COMPL_ENT: TIBStringField
      FieldName = 'COMPL_ENT'
      Origin = '"CAD_NFV"."COMPL_ENT"'
      Size = 10
    end
    object IBTable1TOTA_ITEM: TIBBCDField
      FieldName = 'TOTA_ITEM'
      Origin = '"CAD_NFV"."TOTA_ITEM"'
      Precision = 18
      Size = 2
    end
    object IBTable1VALO_TOTA: TIBBCDField
      FieldName = 'VALO_TOTA'
      Origin = '"CAD_NFV"."VALO_TOTA"'
      Precision = 18
      Size = 2
    end
    object IBTable1OBS_PAG: TWideMemoField
      FieldName = 'OBS_PAG'
      Origin = '"CAD_NFV"."OBS_PAG"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1OBS_GER: TWideMemoField
      FieldName = 'OBS_GER'
      Origin = '"CAD_NFV"."OBS_GER"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1ESTOQUE: TIBStringField
      FieldName = 'ESTOQUE'
      Origin = '"CAD_NFV"."ESTOQUE"'
      Size = 1
    end
    object IBTable1OBS_NF: TWideMemoField
      FieldName = 'OBS_NF'
      Origin = '"CAD_NFV"."OBS_NF"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1MARCA: TIBStringField
      FieldName = 'MARCA'
      Origin = '"CAD_NFV"."MARCA"'
      Size = 15
    end
    object IBTable1NUMERO: TIBStringField
      FieldName = 'NUMERO'
      Origin = '"CAD_NFV"."NUMERO"'
      Size = 10
    end
    object IBTable1QUANT: TIBBCDField
      FieldName = 'QUANT'
      Origin = '"CAD_NFV"."QUANT"'
      Precision = 18
      Size = 2
    end
    object IBTable1ESPECIE: TIBStringField
      FieldName = 'ESPECIE'
      Origin = '"CAD_NFV"."ESPECIE"'
      Size = 15
    end
    object IBTable1PESO_BRUT: TIBBCDField
      FieldName = 'PESO_BRUT'
      Origin = '"CAD_NFV"."PESO_BRUT"'
      Precision = 18
      Size = 2
    end
    object IBTable1PESO_LIQU: TIBBCDField
      FieldName = 'PESO_LIQU'
      Origin = '"CAD_NFV"."PESO_LIQU"'
      Precision = 18
      Size = 2
    end
    object IBTable1COD_TRN: TIntegerField
      FieldName = 'COD_TRN'
      Origin = '"CAD_NFV"."COD_TRN"'
    end
    object IBTable1TIPO_FCI: TIBStringField
      FieldName = 'TIPO_FCI'
      Origin = '"CAD_NFV"."TIPO_FCI"'
      Size = 1
    end
    object IBTable1BASE_CALC: TIBBCDField
      FieldName = 'BASE_CALC'
      Origin = '"CAD_NFV"."BASE_CALC"'
      Precision = 18
      Size = 2
    end
    object IBTable1TOT_VEN: TIBBCDField
      FieldName = 'TOT_VEN'
      Origin = '"CAD_NFV"."TOT_VEN"'
      Precision = 18
      Size = 2
    end
    object IBTable1TOT_SER: TIBBCDField
      FieldName = 'TOT_SER'
      Origin = '"CAD_NFV"."TOT_SER"'
      Precision = 18
      Size = 2
    end
    object IBTable1TOT_ICM: TIBBCDField
      FieldName = 'TOT_ICM'
      Origin = '"CAD_NFV"."TOT_ICM"'
      Precision = 18
      Size = 2
    end
    object IBTable1TOT_IPI: TIBBCDField
      FieldName = 'TOT_IPI'
      Origin = '"CAD_NFV"."TOT_IPI"'
      Precision = 18
      Size = 2
    end
    object IBTable1TOT_ISS: TIBBCDField
      FieldName = 'TOT_ISS'
      Origin = '"CAD_NFV"."TOT_ISS"'
      Precision = 18
      Size = 2
    end
    object IBTable1TIPO_FRETE: TSmallintField
      FieldName = 'TIPO_FRETE'
      Origin = '"CAD_NFV"."TIPO_FRETE"'
    end
    object IBTable1DATA_NOFI: TDateField
      FieldName = 'DATA_NOFI'
      Origin = '"CAD_NFV"."DATA_NOFI"'
    end
    object IBTable1DATA_VENDA: TDateField
      FieldName = 'DATA_VENDA'
      Origin = '"CAD_NFV"."DATA_VENDA"'
    end
    object IBTable1DATA_INI: TDateField
      FieldName = 'DATA_INI'
      Origin = '"CAD_NFV"."DATA_INI"'
    end
    object IBTable1DATA_ENTR: TDateField
      FieldName = 'DATA_ENTR'
      Origin = '"CAD_NFV"."DATA_ENTR"'
    end
    object IBTable1DATA_PEDI: TDateField
      FieldName = 'DATA_PEDI'
      Origin = '"CAD_NFV"."DATA_PEDI"'
    end
    object IBTable1SITUACAO: TIBStringField
      FieldName = 'SITUACAO'
      Origin = '"CAD_NFV"."SITUACAO"'
      Size = 4
    end
    object IBTable1DOLAR: TIBBCDField
      FieldName = 'DOLAR'
      Origin = '"CAD_NFV"."DOLAR"'
      Precision = 18
      Size = 2
    end
    object IBTable1FONE1_FAT: TIBStringField
      FieldName = 'FONE1_FAT'
      Origin = '"CAD_NFV"."FONE1_FAT"'
      Size = 14
    end
    object IBTable1FONE2_FAT: TIBStringField
      FieldName = 'FONE2_FAT'
      Origin = '"CAD_NFV"."FONE2_FAT"'
      Size = 14
    end
    object IBTable1FONE1_COB: TIBStringField
      FieldName = 'FONE1_COB'
      Origin = '"CAD_NFV"."FONE1_COB"'
      Size = 14
    end
    object IBTable1FONE2_COB: TIBStringField
      FieldName = 'FONE2_COB'
      Origin = '"CAD_NFV"."FONE2_COB"'
      Size = 14
    end
    object IBTable1FONE1_COM: TIBStringField
      FieldName = 'FONE1_COM'
      Origin = '"CAD_NFV"."FONE1_COM"'
      Size = 14
    end
    object IBTable1FONE2_COM: TIBStringField
      FieldName = 'FONE2_COM'
      Origin = '"CAD_NFV"."FONE2_COM"'
      Size = 14
    end
    object IBTable1FONE1_ENT: TIBStringField
      FieldName = 'FONE1_ENT'
      Origin = '"CAD_NFV"."FONE1_ENT"'
      Size = 14
    end
    object IBTable1FONE2_ENT: TIBStringField
      FieldName = 'FONE2_ENT'
      Origin = '"CAD_NFV"."FONE2_ENT"'
      Size = 14
    end
    object IBTable1NUM_PCL: TIBStringField
      FieldName = 'NUM_PCL'
      Origin = '"CAD_NFV"."NUM_PCL"'
    end
    object IBTable1CONTAT_FAT: TIBStringField
      FieldName = 'CONTAT_FAT'
      Origin = '"CAD_NFV"."CONTAT_FAT"'
      Size = 30
    end
    object IBTable1CONTAT_COB: TIBStringField
      FieldName = 'CONTAT_COB'
      Origin = '"CAD_NFV"."CONTAT_COB"'
      Size = 30
    end
    object IBTable1CONTAT_COM: TIBStringField
      FieldName = 'CONTAT_COM'
      Origin = '"CAD_NFV"."CONTAT_COM"'
      Size = 30
    end
    object IBTable1CONTAT_ENT: TIBStringField
      FieldName = 'CONTAT_ENT'
      Origin = '"CAD_NFV"."CONTAT_ENT"'
      Size = 30
    end
    object IBTable1OBS_PED: TWideMemoField
      FieldName = 'OBS_PED'
      Origin = '"CAD_NFV"."OBS_PED"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
    object IBTable1ESPECIAL: TIBStringField
      FieldName = 'ESPECIAL'
      Origin = '"CAD_NFV"."ESPECIAL"'
      Size = 1
    end
    object IBTable1FRETE: TIBBCDField
      FieldName = 'FRETE'
      Origin = '"CAD_NFV"."FRETE"'
      Precision = 18
      Size = 2
    end
    object IBTable1SEGURO: TIBBCDField
      FieldName = 'SEGURO'
      Origin = '"CAD_NFV"."SEGURO"'
      Precision = 18
      Size = 2
    end
    object IBTable1OUTROS: TIBBCDField
      FieldName = 'OUTROS'
      Origin = '"CAD_NFV"."OUTROS"'
      Precision = 18
      Size = 2
    end
    object IBTable1MOTIVO: TIBStringField
      FieldName = 'MOTIVO'
      Origin = '"CAD_NFV"."MOTIVO"'
      Size = 100
    end
    object IBTable1IRRF: TIBBCDField
      FieldName = 'IRRF'
      Origin = '"CAD_NFV"."IRRF"'
      Precision = 9
      Size = 2
    end
    object IBTable1PERC_ISS: TIBBCDField
      FieldName = 'PERC_ISS'
      Origin = '"CAD_NFV"."PERC_ISS"'
      Precision = 4
      Size = 2
    end
    object IBTable1TIPO_CLIE: TIBStringField
      FieldName = 'TIPO_CLIE'
      Origin = '"CAD_NFV"."TIPO_CLIE"'
      Size = 1
    end
    object IBTable1CAR_COR: TIBStringField
      FieldName = 'CAR_COR'
      Origin = '"CAD_NFV"."CAR_COR"'
      Size = 1
    end
    object IBTable1DESC_ISS: TIBStringField
      FieldName = 'DESC_ISS'
      Origin = '"CAD_NFV"."DESC_ISS"'
      Size = 1
    end
    object IBTable1PAIS_FAT: TIBStringField
      FieldName = 'PAIS_FAT'
      Origin = '"CAD_NFV"."PAIS_FAT"'
      Size = 80
    end
    object IBTable1NUM_FAT: TIBStringField
      FieldName = 'NUM_FAT'
      Origin = '"CAD_NFV"."NUM_FAT"'
      Size = 5
    end
    object IBTable1NUM_COB: TIBStringField
      FieldName = 'NUM_COB'
      Origin = '"CAD_NFV"."NUM_COB"'
      Size = 5
    end
    object IBTable1NUM_COM: TIBStringField
      FieldName = 'NUM_COM'
      Origin = '"CAD_NFV"."NUM_COM"'
      Size = 5
    end
    object IBTable1NUM_ENT: TIBStringField
      FieldName = 'NUM_ENT'
      Origin = '"CAD_NFV"."NUM_ENT"'
      Size = 5
    end
    object IBTable1DESCONTO: TIBBCDField
      FieldName = 'DESCONTO'
      Origin = '"CAD_NFV"."DESCONTO"'
      Precision = 9
      Size = 2
    end
    object IBTable1LOCAL_EMBARQUE: TIBStringField
      FieldName = 'LOCAL_EMBARQUE'
      Origin = '"CAD_NFV"."LOCAL_EMBARQUE"'
      Size = 60
    end
    object IBTable1TOT_II: TIBBCDField
      FieldName = 'TOT_II'
      Origin = '"CAD_NFV"."TOT_II"'
      Precision = 18
      Size = 2
    end
    object IBTable1TX_SISCOMEX_TOTAL: TIBBCDField
      FieldName = 'TX_SISCOMEX_TOTAL'
      Origin = '"CAD_NFV"."TX_SISCOMEX_TOTAL"'
      Precision = 18
      Size = 2
    end
    object IBTable1COD_EMP: TIntegerField
      FieldName = 'COD_EMP'
      Origin = '"CAD_NFV"."COD_EMP"'
    end
  end
end
