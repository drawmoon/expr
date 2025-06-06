grammar Expr;

@members {bool version12=true;}

parse
    : ((unit_statement) SEMICOLON?)* EOF | ((expression) SEMICOLON?)* EOF
    ;

unit_statement
    : transaction_control_statements
    | analyze
    | associate_statistics
    | audit_traditional
    | unified_auditing
    | data_manipulation_language_statements
    | comment_on_column
    | comment_on_table
    | anonymous_block
    | grant_statement
    // | expression
    ;

// DDL -> SQL Statements for Stored SQL Units

// Creation Function - Specific Clauses

parallel_enable_clause
    : PARALLEL_ENABLE partition_by_clause?
    ;

partition_by_clause
    : LEFT_PAREN PARTITION expression BY (ANY | (HASH | RANGE | LIST) paren_column_list) streaming_clause? RIGHT_PAREN
    ;

result_cache_clause
    : RESULT_CACHE relies_on_part?
    ;

relies_on_part
    : RELIES_ON LEFT_PAREN tableview_name (COMMA tableview_name)* RIGHT_PAREN
    ;

streaming_clause
    : (ORDER | CLUSTER) expression BY paren_column_list
    ;

procedure_spec
    : PROCEDURE identifier (LEFT_PAREN parameter ( COMMA parameter )* RIGHT_PAREN)? SEMICOLON
    ;

function_spec
    : FUNCTION identifier (LEFT_PAREN parameter ( COMMA parameter)* RIGHT_PAREN)?
      RETURN type_spec (DETERMINISTIC)? (RESULT_CACHE)? SEMICOLON
    ;

// Procedure DDLs

function_body
    : FUNCTION identifier (LEFT_PAREN parameter (COMMA parameter)* RIGHT_PAREN)?
      RETURN type_spec (invoker_rights_clause | parallel_enable_clause | result_cache_clause | DETERMINISTIC)*
      ((PIPELINED? (IS | AS) (DECLARE? seq_of_declare_specs? body | call_spec)) | (PIPELINED | AGGREGATE) USING implementation_type_name) SEMICOLON
    ;

procedure_body
    : PROCEDURE identifier (LEFT_PAREN parameter (COMMA parameter)* RIGHT_PAREN)? (IS | AS)
      (DECLARE? seq_of_declare_specs? body | call_spec | EXTERNAL) SEMICOLON
    ;

create_index
    : CREATE (UNIQUE | BITMAP)? INDEX index_name
       ON (cluster_index_clause | table_index_clause | bitmap_join_index_clause)
       UNUSABLE?
       SEMICOLON
    ;

cluster_index_clause
    : CLUSTER cluster_name index_attributes?
    ;

cluster_name
    : (id_expression PERIOD)? id_expression
    ;

table_index_clause
    : tableview_name table_alias? LEFT_PAREN (COMMA? index_expr (ASC | DESC)? )+ RIGHT_PAREN
          index_properties?
    ;
bitmap_join_index_clause
    : tableview_name LEFT_PAREN (COMMA? (tableview_name | table_alias)? column_name (ASC | DESC)? )+ RIGHT_PAREN
        FROM (COMMA? tableview_name table_alias)+
        where_clause local_partitioned_index? index_attributes?
    ;

index_expr
    : column_name
    | expression
    ;

index_properties
    : (global_partitioned_index | local_partitioned_index | index_attributes)+
    | INDEXTYPE IS (domain_index_clause | xmlindex_clause)
    ;

domain_index_clause
    : indextype local_domain_index_clause? parallel_clause? (PARAMETERS LEFT_PAREN odci_parameters RIGHT_PAREN )?
    ;

local_domain_index_clause
    : LOCAL (LEFT_PAREN (COMMA? PARTITION partition_name (PARAMETERS LEFT_PAREN odci_parameters RIGHT_PAREN )? )+ RIGHT_PAREN )?
    ;

xmlindex_clause
    : (XDB PERIOD)? XMLINDEX local_xmlindex_clause?
        parallel_clause? //TODO xmlindex_parameters_clause?
    ;

local_xmlindex_clause
    : LOCAL (LEFT_PAREN (COMMA? PARTITION partition_name //TODO xmlindex_parameters_clause?
                                                       )+ RIGHT_PAREN)?
    ;

global_partitioned_index
    : GLOBAL PARTITION BY (RANGE LEFT_PAREN (COMMA? column_name)+ RIGHT_PAREN LEFT_PAREN index_partitioning_clause RIGHT_PAREN
                          | HASH LEFT_PAREN (COMMA? column_name)+ RIGHT_PAREN
                                            (individual_hash_partitions
                                            | hash_partitions_by_quantity
                                            )
                          )
    ;

index_partitioning_clause
    : PARTITION partition_name? VALUES LESS THAN LEFT_PAREN (COMMA? literal)+ RIGHT_PAREN
        segment_attributes_clause?
    ;

local_partitioned_index
    : LOCAL (on_range_partitioned_table
            | on_list_partitioned_table
            | on_hash_partitioned_table
            | on_comp_partitioned_table
            )?
    ;

on_range_partitioned_table
    : LEFT_PAREN (COMMA? PARTITION partition_name?
              ((segment_attributes_clause | key_compression)+ )?
              UNUSABLE? )+
      RIGHT_PAREN
    ;

on_list_partitioned_table
    : LEFT_PAREN (COMMA? PARTITION partition_name?
              ((segment_attributes_clause | key_compression)+ )?
              UNUSABLE? )+
      RIGHT_PAREN
    ;

on_hash_partitioned_table
    : STORE IN LEFT_PAREN (COMMA? tablespace)+ RIGHT_PAREN
    | LEFT_PAREN (COMMA? PARTITION partition_name? (TABLESPACE tablespace)?
                key_compression? UNUSABLE?)+
      RIGHT_PAREN
    ;

on_comp_partitioned_table
    : (STORE IN LEFT_PAREN (COMMA? tablespace)+ RIGHT_PAREN )?
        LEFT_PAREN (COMMA? PARTITION partition_name?
            ((segment_attributes_clause | key_compression)+)?
            UNUSABLE index_subpartition_clause? )+
        RIGHT_PAREN
    ;

index_subpartition_clause
    : STORE IN LEFT_PAREN (COMMA? tablespace)+ RIGHT_PAREN
    | LEFT_PAREN (COMMA? SUBPARTITION subpartition_name? (TABLESPACE tablespace)?
        key_compression? UNUSABLE?)+
      RIGHT_PAREN
    ;

odci_parameters
    : CHAR_STRING
    ;

indextype
    : (id_expression PERIOD)? id_expression
    ;

visible_or_invisible
    : VISIBLE
    | INVISIBLE
    ;

identified_by
    : IDENTIFIED BY id_expression
    ;

// https://docs.oracle.com/cd/E11882_01/server.112/e41084/statements_4005.htm#SQLRF01105
analyze
    : ( ANALYZE (TABLE tableview_name | INDEX index_name) partition_extention_clause?
      | ANALYZE CLUSTER cluster_name
      )

      ( validation_clauses
      | LIST CHAINED ROWS into_clause1?
      | DELETE SYSTEM? STATISTICS
      )
      SEMICOLON
    ;

partition_extention_clause
    : PARTITION ( LEFT_PAREN partition_name RIGHT_PAREN
                | FOR LEFT_PAREN (COMMA? partition_key_value)+ RIGHT_PAREN
                )
    | SUBPARTITION ( LEFT_PAREN subpartition_name RIGHT_PAREN
                   | FOR LEFT_PAREN (COMMA? subpartition_key_value)+ RIGHT_PAREN
                   )
    ;

validation_clauses
    : VALIDATE REF UPDATE (SET DANGLING TO NULL)?
    | VALIDATE STRUCTURE
        ( CASCADE FAST
        | CASCADE online_or_offline? into_clause?
        | CASCADE
        )?
        online_or_offline? into_clause?
    ;

online_or_offline
    : OFFLINE
    | ONLINE
    ;

into_clause1
    : INTO tableview_name?
    ;

//Making assumption on partition ad subpartition key value clauses
partition_key_value
    : literal
    ;

subpartition_key_value
    : literal
    ;

//https://docs.oracle.com/cd/E11882_01/server.112/e41084/statements_4006.htm#SQLRF01106
associate_statistics
    : ASSOCIATE STATISTICS
        WITH (column_association | function_association)
        storage_table_clause?
      SEMICOLON
    ;

column_association
    : COLUMNS (COMMA? tableview_name PERIOD column_name)+ using_statistics_type
    ;

function_association
    : ( FUNCTIONS (COMMA? function_name)+
      | PACKAGES (COMMA? package_name)+
      | TYPES (COMMA? type_name)+
      | INDEXES (COMMA? index_name)+
      | INDEXTYPES (COMMA? indextype_name)+
      )

      ( using_statistics_type
      | default_cost_clause (COMMA default_selectivity_clause)?
      | default_selectivity_clause (COMMA default_cost_clause)?
      )
    ;

indextype_name
    : id_expression
    ;

using_statistics_type
    : USING (statistics_type_name | NULL)
    ;

statistics_type_name
    : regular_id
    ;

default_cost_clause
    : DEFAULT COST LEFT_PAREN cpu_cost COMMA io_cost COMMA network_cost RIGHT_PAREN
    ;

cpu_cost
    : UNSIGNED_INTEGER
    ;

io_cost
    : UNSIGNED_INTEGER
    ;

network_cost
    : UNSIGNED_INTEGER
    ;

default_selectivity_clause
    : DEFAULT SELECTIVITY default_selectivity
    ;

default_selectivity
    : UNSIGNED_INTEGER
    ;

storage_table_clause
    : WITH (SYSTEM | USER) MANAGED STORAGE TABLES
    ;

// https://docs.oracle.com/database/121/SQLRF/statements_4008.htm#SQLRF56110
unified_auditing
    : {version12}?
      AUDIT (POLICY policy_name ((BY | EXCEPT) (COMMA? audit_user)+ )?
                                (WHENEVER NOT? SUCCESSFUL)?
            | CONTEXT NAMESPACE oracle_namespace
                      ATTRIBUTES (COMMA? attribute_name)+ (BY (COMMA? audit_user)+)?
            )
      SEMICOLON
    ;

policy_name
    : identifier
    ;

// https://docs.oracle.com/cd/E11882_01/server.112/e41084/statements_4007.htm#SQLRF01107
// https://docs.oracle.com/database/121/SQLRF/statements_4007.htm#SQLRF01107

audit_traditional
    : AUDIT ( audit_operation_clause (auditing_by_clause | IN SESSION CURRENT)?
            | audit_schema_object_clause
            | NETWORK
            | audit_direct_path
            )
        (BY (SESSION | ACCESS) )? (WHENEVER NOT? SUCCESSFUL)?
        audit_container_clause?
      SEMICOLON
    ;

audit_direct_path
    : {version12}? DIRECT_PATH auditing_by_clause
    ;

audit_container_clause
    : {version12}? (CONTAINER EQUALS_OP (CURRENT | ALL))
    ;

audit_operation_clause
    : ( (COMMA? (sql_statement_shortcut | ALL STATEMENTS?) )+
      | (COMMA? (system_privilege | ALL PRIVILEGES) )+
      )
    ;

auditing_by_clause
    : BY (COMMA? audit_user)+
    ;

audit_user
    : regular_id
    ;

audit_schema_object_clause
    : ( (COMMA? sql_operation)+ | ALL) auditing_on_clause
    ;

sql_operation
    : ALTER
    | AUDIT
    | COMMENT
    | DELETE
    | EXECUTE
    | FLASHBACK
    | GRANT
    | INDEX
    | INSERT
    | LOCK
    | READ
    | RENAME
    | SELECT
    | UPDATE
    ;

auditing_on_clause
    : ON ( object_name
         | DIRECTORY regular_id
         | MINING MODEL model_name
         | {version12}? SQL TRANSLATION PROFILE profile_name
         | DEFAULT
         )
    ;

model_name
    : (id_expression PERIOD)? id_expression
    ;

object_name
    : (id_expression PERIOD)? id_expression
    ;

profile_name
    : (id_expression PERIOD)? id_expression
    ;

sql_statement_shortcut
    : ALTER SYSTEM
    | CLUSTER
    | CONTEXT
    | DATABASE LINK
    | DIMENSION
    | DIRECTORY
    | INDEX
    | MATERIALIZED VIEW
    | NOT EXISTS
    | OUTLINE
    | {version12}? PLUGGABLE DATABASE
    | PROCEDURE
    | PROFILE
    | PUBLIC DATABASE LINK
    | PUBLIC SYNONYM
    | ROLE
    | ROLLBACK SEGMENT
    | SEQUENCE
    | SESSION
    | SYNONYM
    | SYSTEM AUDIT
    | SYSTEM GRANT
    | TABLE
    | TABLESPACE
    | TRIGGER
    | TYPE
    | USER
    | VIEW
    | ALTER SEQUENCE
    | ALTER TABLE
    | COMMENT TABLE
    | DELETE TABLE
    | EXECUTE PROCEDURE
    | GRANT DIRECTORY
    | GRANT PROCEDURE
    | GRANT SEQUENCE
    | GRANT TABLE
    | GRANT TYPE
    | INSERT TABLE
    | LOCK TABLE
    | SELECT SEQUENCE
    | SELECT TABLE
    | UPDATE TABLE
    ;

grant_statement
    : GRANT
        ( COMMA?
          (role_name
          | system_privilege
          | object_privilege paren_column_list?
          )
        )+
      (ON grant_object_name)?
      TO (COMMA? grantee_name | PUBLIC)+
      (WITH (ADMIN | DELEGATE) OPTION)?
      (WITH HIERARCHY OPTION)?
      (WITH GRANT OPTION)?
      container_clause? SEMICOLON
    ;

container_clause
    : CONTAINER EQUALS_OP (CURRENT | ALL)
    ;

inline_constraint
    : (CONSTRAINT constraint_name)?
        ( NOT? NULL
        | UNIQUE
        | PRIMARY KEY
        | references_clause
        | check_constraint
        )
      constraint_state?
    ;

inline_ref_constraint
    : SCOPE IS tableview_name
    | WITH ROWID
    | (CONSTRAINT constraint_name)? references_clause constraint_state?
    ;

out_of_line_ref_constraint
    : SCOPE FOR LEFT_PAREN ref_col_or_attr=regular_id RIGHT_PAREN IS tableview_name
    | REF LEFT_PAREN ref_col_or_attr=regular_id RIGHT_PAREN WITH ROWID
    | (CONSTRAINT constraint_name)? FOREIGN KEY LEFT_PAREN ( COMMA? ref_col_or_attr=regular_id)+ RIGHT_PAREN references_clause constraint_state?
    ;

out_of_line_constraint
    : ( (CONSTRAINT constraint_name)?
          ( UNIQUE LEFT_PAREN (COMMA? column_name)+ RIGHT_PAREN
          | PRIMARY KEY LEFT_PAREN (COMMA? column_name)+ RIGHT_PAREN
          | foreign_key_clause
          | CHECK LEFT_PAREN expression RIGHT_PAREN
          )
       )
      constraint_state?
    ;

constraint_state
    : ( NOT? DEFERRABLE
      | INITIALLY (IMMEDIATE|DEFERRED)
      | (RELY|NORELY)
      | (ENABLE|DISABLE)
      | (VALIDATE|NOVALIDATE)
      | using_index_clause
      )+
    ;

logging_clause
    : LOGGING
     | NOLOGGING
     | FILESYSTEM_LIKE_LOGGING
    ;

parallel_clause
    : NOPARALLEL
    | PARALLEL parallel_count=UNSIGNED_INTEGER?
    ;

oracle_namespace
    : id_expression
    ;

xmltype_column_properties
    : XMLTYPE COLUMN? column_name xmltype_storage? xmlschema_spec?
    ;

xmltype_storage
    : STORE  AS (OBJECT RELATIONAL
                | (SECUREFILE | BASICFILE)? (CLOB | BINARY XML) (lob_segname (LEFT_PAREN lob_parameters RIGHT_PAREN)? | LEFT_PAREN lob_parameters RIGHT_PAREN)?
                )
    | STORE VARRAYS AS (LOBS | TABLES)
    ;

xmlschema_spec
    : (XMLSCHEMA DELIMITED_ID)? ELEMENT DELIMITED_ID
         (allow_or_disallow NONSCHEMA)?
         (allow_or_disallow ANYSCHEMA)?
    ;

object_properties
    : (column_name | attribute_name) (DEFAULT expression)? ((COMMA? inline_constraint)+ | inline_ref_constraint)?
    | out_of_line_constraint
    | out_of_line_ref_constraint
    | supplemental_logging_props
    ;

individual_hash_partitions
    : LEFT_PAREN (COMMA? PARTITION partition_name? partitioning_storage_clause?)+ RIGHT_PAREN
    ;

hash_partitions_by_quantity
    : PARTITIONS hash_partition_quantity
       (STORE IN LEFT_PAREN (COMMA? tablespace)+ RIGHT_PAREN)?
         (table_compression | key_compression)?
         (OVERFLOW_ STORE IN LEFT_PAREN (COMMA? tablespace)+ RIGHT_PAREN )?
    ;

hash_partition_quantity
    : UNSIGNED_INTEGER
    ;

subpartition_name
    : partition_name
    ;

partitioning_storage_clause
    : ( TABLESPACE tablespace
      | OVERFLOW_ (TABLESPACE tablespace)?
      | table_compression
      | key_compression
      | lob_partitioning_storage
      | VARRAY varray_item STORE AS (BASICFILE | SECUREFILE)? LOB lob_segname
      )+
    ;

lob_partitioning_storage
    : LOB LEFT_PAREN lob_item RIGHT_PAREN
       STORE AS (BASICFILE | SECUREFILE)?
               (lob_segname (LEFT_PAREN TABLESPACE tablespace RIGHT_PAREN )?
               | LEFT_PAREN TABLESPACE tablespace RIGHT_PAREN
               )
    ;

//Technically, this should only allow 'K' | 'M' | 'G' | 'T' | 'P' | 'E'
// but having issues with examples/numbers01.sql line 11 "sysdate -1m"
size_clause
    : UNSIGNED_INTEGER REGULAR_ID?
    ;

table_compression
    : COMPRESS
        ( BASIC
        | FOR ( OLTP
              | (QUERY | ARCHIVE) (LOW | HIGH)?
              )
        )?
    | NOCOMPRESS
    ;

physical_attributes_clause
    : (PCTFREE pctfree=UNSIGNED_INTEGER
      | PCTUSED pctused=UNSIGNED_INTEGER
      | INITRANS inittrans=UNSIGNED_INTEGER
      | storage_clause
      )+
    ;

storage_clause
    : STORAGE LEFT_PAREN
         (INITIAL initial_size=size_clause
         | NEXT next_size=size_clause
         | MINEXTENTS minextents=(UNSIGNED_INTEGER | UNLIMITED)
         | MAXEXTENTS minextents=(UNSIGNED_INTEGER | UNLIMITED)
         | PCTINCREASE pctincrease=UNSIGNED_INTEGER
         | FREELISTS freelists=UNSIGNED_INTEGER
         | FREELIST GROUPS freelist_groups=UNSIGNED_INTEGER
         | OPTIMAL (size_clause | NULL )
         | BUFFER_POOL (KEEP | RECYCLE | DEFAULT)
         | FLASH_CACHE (KEEP | NONE | DEFAULT)
         | ENCRYPT
         )+
       RIGHT_PAREN
    ;

deferred_segment_creation
    : SEGMENT CREATION (IMMEDIATE | DEFERRED)
    ;

segment_attributes_clause
    : ( physical_attributes_clause
      | TABLESPACE tablespace_name=id_expression
      | logging_clause
      )+
    ;

physical_properties
    : deferred_segment_creation?  segment_attributes_clause table_compression?
    ;

log_grp
    : UNSIGNED_INTEGER
    ;

supplemental_log_grp_clause
    : GROUP log_grp LEFT_PAREN (COMMA? regular_id (NO LOG)?)+ RIGHT_PAREN ALWAYS?
    ;

supplemental_id_key_clause
    : DATA LEFT_PAREN( COMMA? ( ALL
                     | PRIMARY KEY
                     | UNIQUE
                     | FOREIGN KEY
                     )
              )+
           RIGHT_PAREN
      COLUMNS
    ;

comment_on_column
    : COMMENT ON COLUMN tableview_name PERIOD column_name IS quoted_string
    ;

allow_or_disallow
    : ALLOW
    | DISALLOW
    ;

comment_on_table
    : COMMENT ON TABLE tableview_name IS quoted_string
    ;

using_index_clause
    : USING INDEX (index_name | LEFT_PAREN create_index RIGHT_PAREN | index_attributes )?
    ;

index_attributes
    : ( physical_attributes_clause
      | logging_clause
      | TABLESPACE (tablespace | DEFAULT)
      | key_compression
      | sort_or_nosort
      | REVERSE
      | visible_or_invisible
      | parallel_clause
      )+
    ;

sort_or_nosort
    : SORT
    | NOSORT
    ;

key_compression
    : NOCOMPRESS
    | COMPRESS UNSIGNED_INTEGER
    ;

varray_col_properties
    : VARRAY varray_item ( substitutable_column_clause? varray_storage_clause
                         | substitutable_column_clause
                         )
    ;

varray_storage_clause
    : STORE AS (SECUREFILE|BASICFILE)? LOB ( lob_segname? LEFT_PAREN lob_storage_parameters RIGHT_PAREN
                                           | lob_segname
                                           )
    ;

lob_segname
    : regular_id
    ;

lob_item
    : regular_id
    ;

lob_storage_parameters
    :  TABLESPACE tablespace | (lob_parameters storage_clause? )
    |  storage_clause
    ;

lob_storage_clause
    : LOB ( LEFT_PAREN (COMMA? lob_item)+ RIGHT_PAREN STORE AS ( (SECUREFILE|BASICFILE) | LEFT_PAREN lob_storage_parameters RIGHT_PAREN )+
          | LEFT_PAREN lob_item RIGHT_PAREN STORE AS ( (SECUREFILE | BASICFILE) | lob_segname | LEFT_PAREN lob_storage_parameters RIGHT_PAREN )+
          )
    ;

lob_parameters
    : ( (ENABLE | DISABLE) STORAGE IN ROW
      | CHUNK UNSIGNED_INTEGER
      | PCTVERSION UNSIGNED_INTEGER
      | FREEPOOLS UNSIGNED_INTEGER
      | lob_retention_clause
      | lob_deduplicate_clause
      | lob_compression_clause
      | ENCRYPT encryption_spec
      | DECRYPT
      | (CACHE | NOCACHE | CACHE READS) logging_clause?
      )+
    ;

lob_deduplicate_clause
    : DEDUPLICATE
    | KEEP_DUPLICATES
    ;

lob_compression_clause
    : NOCOMPRESS
    | COMPRESS (HIGH | MEDIUM | LOW)?
    ;

lob_retention_clause
    : RETENTION (MAX | MIN UNSIGNED_INTEGER | AUTO | NONE)?
    ;

encryption_spec
    : (USING  CHAR_STRING)? (IDENTIFIED BY REGULAR_ID)? CHAR_STRING? (NO? SALT)?
    ;
tablespace
    : regular_id
    ;

varray_item
    : (id_expression PERIOD)? (id_expression PERIOD)? id_expression
    ;

column_properties
    : object_type_col_properties
    | nested_table_col_properties
    | (varray_col_properties | lob_storage_clause) //TODO LEFT_PAREN ( COMMA? lob_partition_storage)+ RIGHT_PAREN
    | xmltype_column_properties
    ;

nested_table_col_properties
    : NESTED TABLE  (nested_item | COLUMN_VALUE) substitutable_column_clause? (LOCAL | GLOBAL)?
       STORE AS tableview_name ( LEFT_PAREN ( LEFT_PAREN object_properties RIGHT_PAREN
                                     | physical_properties
                                     | column_properties
                                     )+
                                  RIGHT_PAREN
                               )?
        (RETURN AS? (LOCATOR | VALUE) )?
     ;

nested_item
    : regular_id
    ;

substitutable_column_clause
    : ELEMENT? IS OF TYPE? LEFT_PAREN type_name RIGHT_PAREN
    | NOT? SUBSTITUTABLE AT ALL LEVELS
    ;

partition_name
    : regular_id
    ;

supplemental_logging_props
    : SUPPLEMENTAL LOG (supplemental_log_grp_clause | supplemental_id_key_clause)
    ;

object_type_col_properties
    : COLUMN column=regular_id substitutable_column_clause
    ;

check_constraint
    : CHECK LEFT_PAREN condition RIGHT_PAREN DISABLE?
    ;

foreign_key_clause
    : FOREIGN KEY paren_column_list references_clause on_delete_clause?
    ;

references_clause
    : REFERENCES tableview_name paren_column_list
    ;

on_delete_clause
    : ON DELETE (CASCADE | SET NULL)
    ;

// Anonymous SQL code block

anonymous_block
    : (DECLARE seq_of_declare_specs)? BEGIN seq_of_statements (EXCEPTION exception_handler+)? END SEMICOLON
    ;

// Common DDL Clauses

invoker_rights_clause
    : AUTHID (CURRENT_USER | DEFINER)
    ;

call_spec
    : LANGUAGE (java_spec | c_spec)
    ;

// Call Spec Specific Clauses

java_spec
    : JAVA NAME CHAR_STRING
    ;

c_spec
    : C_LETTER (NAME CHAR_STRING)? LIBRARY identifier c_agent_in_clause? (WITH CONTEXT)? c_parameters_clause?
    ;

c_agent_in_clause
    : AGENT IN LEFT_PAREN expressions RIGHT_PAREN
    ;

c_parameters_clause
    : PARAMETERS LEFT_PAREN (expressions | PERIOD PERIOD PERIOD) RIGHT_PAREN
    ;

parameter
    : parameter_name (IN | OUT | INOUT | NOCOPY)* type_spec? default_value_part?
    ;

default_value_part
    : (ASSIGN_OP | DEFAULT) expression
    ;

// Elements Declarations

seq_of_declare_specs
    : declare_spec+
    ;

declare_spec
    : pragma_declaration
    | variable_declaration
    | subtype_declaration
    | cursor_declaration
    | exception_declaration
    | type_declaration
    | procedure_spec
    | function_spec
    | procedure_body
    | function_body
    ;

// incorporates constant_declaration
variable_declaration
    : identifier CONSTANT? type_spec (NOT NULL)? default_value_part? SEMICOLON
    ;

subtype_declaration
    : SUBTYPE identifier IS type_spec (RANGE expression DOUBLE_PERIOD expression)? (NOT NULL)? SEMICOLON
    ;

// cursor_declaration incorportates curscursor_body and cursor_spec

cursor_declaration
    : CURSOR identifier (LEFT_PAREN (COMMA? parameter_spec)+ RIGHT_PAREN )? (RETURN type_spec)? (IS select_statement)? SEMICOLON
    ;

parameter_spec
    : parameter_name (IN? type_spec)? default_value_part?
    ;

exception_declaration
    : identifier EXCEPTION SEMICOLON
    ;

pragma_declaration
    : PRAGMA (SERIALLY_REUSABLE
    | AUTONOMOUS_TRANSACTION
    | EXCEPTION_INIT LEFT_PAREN exception_name COMMA numeric_negative RIGHT_PAREN
    | INLINE LEFT_PAREN id1=identifier COMMA expression RIGHT_PAREN
    | RESTRICT_REFERENCES LEFT_PAREN (identifier | DEFAULT) (COMMA identifier)+ RIGHT_PAREN) SEMICOLON
    ;

// Record Declaration Specific Clauses

// incorporates ref_cursor_type_definition

record_type_def
    : RECORD LEFT_PAREN (COMMA? field_spec)+ RIGHT_PAREN
    ;

field_spec
    : column_name type_spec? (NOT NULL)? default_value_part?
    ;

ref_cursor_type_def
    : REF CURSOR (RETURN type_spec)?
    ;

type_declaration
    : TYPE identifier IS (table_type_def | varray_type_def | record_type_def | ref_cursor_type_def) SEMICOLON
    ;

table_type_def
    : TABLE OF type_spec table_indexed_by_part? (NOT NULL)?
    ;

table_indexed_by_part
    : (idx1=INDEXED | idx2=INDEX) BY type_spec
    ;

varray_type_def
    : (VARRAY | VARYING ARRAY) LEFT_PAREN expression RIGHT_PAREN OF type_spec (NOT NULL)?
    ;

// Statements

seq_of_statements
    : (statement (SEMICOLON | EOF) | label_declaration)+
    ;

label_declaration
    : ltp1= LESS_THAN_OP LESS_THAN_OP label_name GREATER_THAN_OP GREATER_THAN_OP
    ;

statement
    : CREATE swallow_to_semi
    | TRUNCATE swallow_to_semi
    | body
    | block
    | assignment_statement
    | continue_statement
    | exit_statement
    | goto_statement
    | if_statement
    | loop_statement
    | forall_statement
    | null_statement
    | raise_statement
    | return_statement
    | case_statement/*[true]*/
    | sql_statement
    | function_call
    | pipe_row_statement
    ;

swallow_to_semi
    : ~SEMICOLON+
    ;

assignment_statement
    : (general_element | bind_variable) ASSIGN_OP expression
    ;

continue_statement
    : CONTINUE label_name? (WHEN condition)?
    ;

exit_statement
    : EXIT label_name? (WHEN condition)?
    ;

goto_statement
    : GOTO label_name
    ;

if_statement
    : IF condition THEN seq_of_statements elsif_part* else_part? END IF
    ;

elsif_part
    : ELSIF condition THEN seq_of_statements
    ;

else_part
    : ELSE seq_of_statements
    ;

loop_statement
    : label_declaration? (WHILE condition | FOR cursor_loop_param)? LOOP seq_of_statements END LOOP label_name?
    ;

// Loop Specific Clause

cursor_loop_param
    : index_name IN REVERSE? lower_bound range_separator=DOUBLE_PERIOD upper_bound
    | record_name IN (cursor_name (LEFT_PAREN expressions? RIGHT_PAREN)? | LEFT_PAREN select_statement RIGHT_PAREN)
    ;

forall_statement
    : FORALL index_name IN bounds_clause sql_statement (SAVE EXCEPTIONS)?
    ;

bounds_clause
    : lower_bound DOUBLE_PERIOD upper_bound
    | INDICES OF collection_name between_bound?
    | VALUES OF index_name
    ;

between_bound
    : BETWEEN lower_bound AND upper_bound
    ;

lower_bound
    : concatenation
    ;

upper_bound
    : concatenation
    ;

null_statement
    : NULL
    ;

raise_statement
    : RAISE exception_name?
    ;

return_statement
    : RETURN expression?
    ;

function_call
    : CALL? routine_name function_argument?
    ;

pipe_row_statement
    : PIPE ROW LEFT_PAREN expression RIGHT_PAREN;

body
    : BEGIN seq_of_statements (EXCEPTION exception_handler+)? END label_name?
    ;

// Body Specific Clause

exception_handler
    : WHEN exception_name (OR exception_name)* THEN seq_of_statements
    ;

block
    : DECLARE? declare_spec+ body
    ;

// SQL Statements

sql_statement
    : execute_immediate
    | data_manipulation_language_statements
    | cursor_manipulation_statements
    | transaction_control_statements
    ;

execute_immediate
    : EXECUTE IMMEDIATE expression (into_clause using_clause? | using_clause dynamic_returning_clause? | dynamic_returning_clause)?
    ;

// Execute Immediate Specific Clause

dynamic_returning_clause
    : (RETURNING | RETURN) into_clause
    ;

// DML Statements

data_manipulation_language_statements
    : merge_statement
    | lock_table_statement
    | select_statement
    | update_statement
    | delete_statement
    | insert_statement
    | explain_statement
    ;

// Cursor Manipulation Statements

cursor_manipulation_statements
    : close_statement
    | open_statement
    | fetch_statement
    | open_for_statement
    ;

close_statement
    : CLOSE cursor_name
    ;

open_statement
    : OPEN cursor_name (LEFT_PAREN expressions? RIGHT_PAREN)?
    ;

fetch_statement
    : FETCH cursor_name (it1=INTO (COMMA? variable_name)+ | BULK COLLECT INTO (COMMA? variable_name)+)
    ;

open_for_statement
    : OPEN variable_name FOR (select_statement | expression) using_clause?
    ;

// Transaction Control SQL Statements

transaction_control_statements
    : set_transaction_command
    | set_constraint_command
    | commit_statement
    | rollback_statement
    | savepoint_statement
    ;

set_transaction_command
    : SET TRANSACTION
      (READ (ONLY | WRITE) | ISOLATION LEVEL (SERIALIZABLE | READ COMMITTED) | USE ROLLBACK SEGMENT rollback_segment_name)?
      (NAME quoted_string)?
    ;

set_constraint_command
    : SET (CONSTRAINT | CONSTRAINTS) (ALL | (COMMA? constraint_name)+) (IMMEDIATE | DEFERRED)
    ;

commit_statement
    : COMMIT WORK?
      (COMMENT expression | FORCE (CORRUPT_XID expression | CORRUPT_XID_ALL | expression (COMMA expression)?))?
      write_clause?
    ;

write_clause
    : WRITE (WAIT | NOWAIT)? (IMMEDIATE | BATCH)?
    ;

rollback_statement
    : ROLLBACK WORK? (TO SAVEPOINT? savepoint_name | FORCE quoted_string)?
    ;

savepoint_statement
    : SAVEPOINT savepoint_name
    ;

// Dml

/* TODO
//SHOULD BE OVERRIDEN!
compilation_unit
    : seq_of_statements* EOF
    ;

//SHOULD BE OVERRIDEN!
seq_of_statements
    : select_statement
    | update_statement
    | delete_statement
    | insert_statement
    | lock_table_statement
    | merge_statement
    | explain_statement
//    | case_statement[true]
    ;
*/

explain_statement
    : EXPLAIN PLAN (SET STATEMENT_ID EQUALS_OP quoted_string)? (INTO tableview_name)?
      FOR (select_statement | update_statement | delete_statement | insert_statement | merge_statement)
    ;

select_statement
    : subquery_factoring_clause? subquery (for_update_clause | order_by_clause | offset_clause | fetch_clause)*
    ;

// Select Specific Clauses

subquery_factoring_clause
    : WITH factoring_element (COMMA factoring_element)*
    ;

factoring_element
    : query_name paren_column_list? AS LEFT_PAREN subquery order_by_clause? RIGHT_PAREN
      search_clause? cycle_clause?
    ;

search_clause
    : SEARCH (DEPTH | BREADTH) FIRST BY column_name ASC? DESC? (NULLS FIRST)? (NULLS LAST)?
      (COMMA column_name ASC? DESC? (NULLS FIRST)? (NULLS LAST)?)* SET column_name
    ;

cycle_clause
    : CYCLE column_list SET column_name TO expression DEFAULT expression
    ;

subquery
    : subquery_basic_elements subquery_operation_part*
    ;

subquery_basic_elements
    : query_block
    | LEFT_PAREN subquery RIGHT_PAREN
    ;

subquery_operation_part
    : (UNION ALL? | INTERSECT | EXCEPT| MINUS) subquery_basic_elements
    ;

query_block
    : SELECT (DISTINCT | UNIQUE | ALL)? (ASTERISK | (COMMA? selected_element)+)
      into_clause? from_clause? where_clause? hierarchical_query_clause? group_by_clause? model_clause?
    ;

selected_element
    : select_list_elements column_alias?
    ;

from_clause
    : FROM table_ref_list
    ;

select_list_elements
    : tableview_name PERIOD ASTERISK
    | (regular_id PERIOD)? expression // expressions ?
    ;

table_ref_list
    : (COMMA? table_ref)+
    ;

// NOTE to PIVOT clause
// according the SQL reference this should not be possible
// according to he reality it is. Here we probably apply pivot/unpivot onto whole join clause
// eventhough it is not enclosed in parenthesis. See pivot examples 09,10,11

table_ref
    : table_ref_aux join_clause* (pivot_clause | unpivot_clause)?
    ;

table_ref_aux
    : table_ref_aux_internal flashback_query_clause* (/*{isTableAlias()}?*/ table_alias)?
    ;

table_ref_aux_internal
    : dml_table_expression_clause (pivot_clause | unpivot_clause)?                # table_ref_aux_internal_one
    | LEFT_PAREN table_ref subquery_operation_part* RIGHT_PAREN (pivot_clause | unpivot_clause)?  # table_ref_aux_internal_two
    | ONLY LEFT_PAREN dml_table_expression_clause RIGHT_PAREN                                     # table_ref_aux_internal_three
    ;

join_clause
    : query_partition_clause? (CROSS | NATURAL)? (INNER | outer_join_type)?
      JOIN table_ref_aux query_partition_clause? (join_on_part | join_using_part)*
    ;

join_on_part
    : ON condition
    ;

join_using_part
    : USING paren_column_list
    ;

outer_join_type
    : (FULL | LEFT | RIGHT) OUTER?
    ;

query_partition_clause
    : PARTITION BY ((LEFT_PAREN (subquery | expressions)? RIGHT_PAREN) | expressions)
    ;

flashback_query_clause
    : VERSIONS BETWEEN (SCN | TIMESTAMP) expression
    | AS OF (SCN | TIMESTAMP | SNAPSHOT) expression
    ;

pivot_clause
    : PIVOT XML? LEFT_PAREN (COMMA? pivot_element)+ pivot_for_clause pivot_in_clause RIGHT_PAREN
    ;

pivot_element
    : aggregate_function_name LEFT_PAREN expression RIGHT_PAREN column_alias?
    ;

pivot_for_clause
    : FOR (column_name | paren_column_list)
    ;

pivot_in_clause
    : IN LEFT_PAREN (subquery | (COMMA? ANY)+ | (COMMA? pivot_in_clause_element)+) RIGHT_PAREN
    ;

pivot_in_clause_element
    : pivot_in_clause_elements column_alias?
    ;

pivot_in_clause_elements
    : expression
    | LEFT_PAREN expressions? RIGHT_PAREN
    ;

unpivot_clause
    : UNPIVOT ((INCLUDE | EXCLUDE) NULLS)?
    LEFT_PAREN (column_name | paren_column_list) pivot_for_clause unpivot_in_clause RIGHT_PAREN
    ;

unpivot_in_clause
    : IN LEFT_PAREN (COMMA? unpivot_in_elements)+ RIGHT_PAREN
    ;

unpivot_in_elements
    : (column_name | paren_column_list)
      (AS (constant | LEFT_PAREN (COMMA? constant)+ RIGHT_PAREN))?
    ;

hierarchical_query_clause
    : CONNECT BY NOCYCLE? condition start_part?
    | start_part CONNECT BY NOCYCLE? condition
    ;

start_part
    : START WITH condition
    ;

group_by_clause
    : GROUP BY (COMMA? group_by_elements)+ having_clause?
    | having_clause (GROUP BY (COMMA? group_by_elements)+)?
    ;

group_by_elements
    : grouping_sets_clause
    | rollup_cube_clause
    | expression
    ;

rollup_cube_clause
    : (ROLLUP | CUBE) LEFT_PAREN (COMMA? grouping_sets_elements)+ RIGHT_PAREN
    ;

grouping_sets_clause
    : GROUPING SETS LEFT_PAREN (COMMA? grouping_sets_elements)+ RIGHT_PAREN
    ;

grouping_sets_elements
    : rollup_cube_clause
    | LEFT_PAREN expressions? RIGHT_PAREN
    | expression
    ;

having_clause
    : HAVING condition
    ;

model_clause
    : MODEL cell_reference_options* return_rows_clause? reference_model* main_model
    ;

cell_reference_options
    : (IGNORE | KEEP) NAV
    | UNIQUE (DIMENSION | SINGLE REFERENCE)
    ;

return_rows_clause
    : RETURN (UPDATED | ALL) ROWS
    ;

reference_model
    : REFERENCE reference_model_name ON LEFT_PAREN subquery RIGHT_PAREN model_column_clauses cell_reference_options*
    ;

main_model
    : (MAIN main_model_name)? model_column_clauses cell_reference_options* model_rules_clause
    ;

model_column_clauses
    : model_column_partition_part? DIMENSION BY model_column_list MEASURES model_column_list
    ;

model_column_partition_part
    : PARTITION BY model_column_list
    ;

model_column_list
    : LEFT_PAREN (COMMA? model_column)+  RIGHT_PAREN
    ;

model_column
    : (expression | query_block) column_alias?
    ;

model_rules_clause
    : model_rules_part? LEFT_PAREN (COMMA? model_rules_element)* RIGHT_PAREN
    ;

model_rules_part
    : RULES (UPDATE | UPSERT ALL?)? ((AUTOMATIC | SEQUENTIAL) ORDER)? model_iterate_clause?
    ;

model_rules_element
    : (UPDATE | UPSERT ALL?)? cell_assignment order_by_clause? EQUALS_OP expression
    ;

cell_assignment
    : model_expression
    ;

model_iterate_clause
    : ITERATE LEFT_PAREN expression RIGHT_PAREN until_part?
    ;

until_part
    : UNTIL LEFT_PAREN condition RIGHT_PAREN
    ;

order_by_clause
    : ORDER SIBLINGS? BY (COMMA? order_by_elements)+
    ;

order_by_elements
    : expression (ASC | DESC)? (NULLS (FIRST | LAST))?
    ;

offset_clause
    : OFFSET expression (ROW | ROWS)
    | SKIP_ numeric
    ;

fetch_clause
    : FETCH (FIRST | NEXT) (expression PERCENT_KEYWORD?)? (ROW | ROWS) (ONLY | WITH TIES)
    | LIMIT numeric
    ;

for_update_clause
    : FOR UPDATE for_update_of_part? for_update_options?
    ;

for_update_of_part
    : OF column_list
    ;

for_update_options
    : SKIP_ LOCKED
    | NOWAIT
    | WAIT expression
    ;

update_statement
    : UPDATE general_table_ref update_set_clause where_clause? static_returning_clause? error_logging_clause?
    ;

// Update Specific Clauses

update_set_clause
    : SET
      ((COMMA? column_based_update_set_clause)+ | VALUE LEFT_PAREN identifier RIGHT_PAREN EQUALS_OP expression)
    ;

column_based_update_set_clause
    : column_name EQUALS_OP expression
    | paren_column_list EQUALS_OP subquery
    ;

delete_statement
    : DELETE FROM? general_table_ref where_clause? static_returning_clause? error_logging_clause?
    ;

insert_statement
    : INSERT (single_table_insert | multi_table_insert)
    ;

// Insert Specific Clauses

single_table_insert
    : insert_into_clause (values_clause static_returning_clause? | select_statement) error_logging_clause?
    ;

multi_table_insert
    : (ALL multi_table_element+ | conditional_insert_clause) select_statement
    ;

multi_table_element
    : insert_into_clause values_clause? error_logging_clause?
    ;

conditional_insert_clause
    : (ALL | FIRST)? conditional_insert_when_part+ conditional_insert_else_part?
    ;

conditional_insert_when_part
    : WHEN condition THEN multi_table_element+
    ;

conditional_insert_else_part
    : ELSE multi_table_element+
    ;

insert_into_clause
    : INTO general_table_ref paren_column_list?
    ;

values_clause
    : VALUES LEFT_PAREN expressions? RIGHT_PAREN
    ;

merge_statement
    : MERGE INTO tableview_name table_alias? USING selected_tableview ON LEFT_PAREN condition RIGHT_PAREN
      (merge_update_clause merge_insert_clause? | merge_insert_clause merge_update_clause?)?
      error_logging_clause?
    ;

// Merge Specific Clauses

merge_update_clause
    : WHEN MATCHED THEN UPDATE SET merge_element (COMMA merge_element)* where_clause? merge_update_delete_part?
    ;

merge_element
    : column_name EQUALS_OP expression
    ;

merge_update_delete_part
    : DELETE where_clause
    ;

merge_insert_clause
    : WHEN NOT MATCHED THEN INSERT paren_column_list?
      VALUES LEFT_PAREN expressions? RIGHT_PAREN where_clause?
    ;

selected_tableview
    : (tableview_name | LEFT_PAREN select_statement RIGHT_PAREN) table_alias?
    ;

lock_table_statement
    : LOCK TABLE lock_table_element (COMMA lock_table_element)* IN lock_mode MODE wait_nowait_part?
    ;

wait_nowait_part
    : WAIT expression
    | NOWAIT
    ;

// Lock Specific Clauses

lock_table_element
    : tableview_name partition_extension_clause?
    ;

lock_mode
    : ROW SHARE
    | ROW EXCLUSIVE
    | SHARE UPDATE?
    | SHARE ROW EXCLUSIVE
    | EXCLUSIVE
    ;

// Common DDL Clauses

general_table_ref
    : (dml_table_expression_clause | ONLY LEFT_PAREN dml_table_expression_clause RIGHT_PAREN) table_alias?
    ;

static_returning_clause
    : (RETURNING | RETURN) expressions into_clause
    ;

error_logging_clause
    : LOG ERRORS error_logging_into_part? expression? error_logging_reject_part?
    ;

error_logging_into_part
    : INTO tableview_name
    ;

error_logging_reject_part
    : REJECT LIMIT (UNLIMITED | expression)
    ;

dml_table_expression_clause
    : table_collection_expression
    | LEFT_PAREN select_statement subquery_restriction_clause? RIGHT_PAREN
    | tableview_name sample_clause?
    ;

table_collection_expression
    : (TABLE | THE) (LEFT_PAREN subquery RIGHT_PAREN | LEFT_PAREN expression RIGHT_PAREN (LEFT_PAREN PLUS_SIGN RIGHT_PAREN)?)
    ;

subquery_restriction_clause
    : WITH (READ ONLY | CHECK OPTION (CONSTRAINT constraint_name)?)
    ;

sample_clause
    : SAMPLE BLOCK? LEFT_PAREN expression (COMMA expression)? RIGHT_PAREN seed_part?
    ;

seed_part
    : SEED LEFT_PAREN expression RIGHT_PAREN
    ;

// Expression & Condition

condition
    : expression
    ;

expressions
    : expression (COMMA expression)*
    ;

expression
    : cursor_expression
    | logical_expression
    ;

cursor_expression
    : CURSOR LEFT_PAREN subquery RIGHT_PAREN
    ;

logical_expression
    : multiset_expression (IS NOT?
        (NULL | NAN_ | PRESENT | INFINITE | A_LETTER SET | EMPTY | OF TYPE?
        LEFT_PAREN ONLY? type_spec (COMMA type_spec)* RIGHT_PAREN))*
    | NOT logical_expression
    | logical_expression AND logical_expression
    | logical_expression OR logical_expression
    ;

multiset_expression
    : relational_expression (multiset_type=(MEMBER | SUBMULTISET) OF? concatenation)?
    ;

relational_expression
    : relational_expression relational_operator relational_expression
    | compound_expression
    ;

compound_expression
    : concatenation
      (NOT? ( IN in_elements
            | BETWEEN between_elements
            | like_type=(LIKE | LIKEC | LIKE2 | LIKE4) concatenation (ESCAPE concatenation)?))?
    ;

relational_operator
    : EQUALS_OP
    | (NOT_EQUAL_OP | LESS_THAN_OP GREATER_THAN_OP | EXCLAMATION_OPERATOR_PART EQUALS_OP | CARRET_OPERATOR_PART EQUALS_OP)
    | (LESS_THAN_OP | GREATER_THAN_OP) EQUALS_OP?
    ;

in_elements
    : LEFT_PAREN subquery RIGHT_PAREN
    | LEFT_PAREN concatenation (COMMA concatenation)* RIGHT_PAREN
    | constant
    | bind_variable
    | general_element
    ;

between_elements
    : concatenation AND concatenation
    ;

concatenation
    : model_expression
        (AT (LOCAL | TIME ZONE concatenation) | interval_expression)?
    | concatenation op=(ASTERISK | SOLIDUS) concatenation
    | concatenation op=(PLUS_SIGN | MINUS_SIGN) concatenation
    | concatenation DOUBLE_COLON type_spec
    | concatenation BAR BAR concatenation
    ;

interval_expression
    : DAY (LEFT_PAREN concatenation RIGHT_PAREN)? TO SECOND (LEFT_PAREN concatenation RIGHT_PAREN)?
    | YEAR (LEFT_PAREN concatenation RIGHT_PAREN)? TO MONTH
    ;

model_expression
    : unary_expression (LEFT_BRACKET model_expression_element RIGHT_BRACKET)?
    ;

model_expression_element
    : (ANY | expression) (COMMA (ANY | expression))*
    | single_column_for_loop (COMMA single_column_for_loop)*
    | multi_column_for_loop
    ;

single_column_for_loop
    : FOR column_name
        ( IN LEFT_PAREN expressions? RIGHT_PAREN
        | (LIKE expression)? FROM fromExpr=expression TO toExpr=expression
            action_type=(INCREMENT | DECREMENT) action_expr=expression)
    ;

multi_column_for_loop
    : FOR paren_column_list
      IN LEFT_PAREN (subquery | LEFT_PAREN expressions? RIGHT_PAREN) RIGHT_PAREN
    ;

unary_expression
    : (MINUS_SIGN | PLUS_SIGN) unary_expression
    | PRIOR unary_expression
    | CONNECT_BY_ROOT unary_expression
    | /*TODO {input.LT(1).getText().equalsIgnoreCase("new") && !input.LT(2).getText().equals(".")}?*/ NEW unary_expression
    | DISTINCT unary_expression
    | ALL unary_expression
    | /*TODO{(input.LA(1) == CASE || input.LA(2) == CASE)}?*/ case_statement/*[false]*/
    | quantified_expression
    | standard_function
    | atom
    ;

case_statement /*TODO [boolean isStatementParameter]
TODO scope    {
    boolean isStatement;
}
@init    {$case_statement::isStatement = $isStatementParameter;}*/
    : searched_case_statement
    | simple_case_statement
    ;

// CASE

simple_case_statement
    : label_name? ck1=CASE expression simple_case_when_part+  case_else_part? END CASE? label_name?
    ;

simple_case_when_part
    : WHEN expression THEN (/*TODO{$case_statement::isStatement}?*/ seq_of_statements | expression)
    ;

searched_case_statement
    : label_name? ck1=CASE searched_case_when_part+ case_else_part? END CASE? label_name?
    ;

searched_case_when_part
    : WHEN expression THEN (/*TODO{$case_statement::isStatement}?*/ seq_of_statements | expression)
    ;

case_else_part
    : ELSE (/*{$case_statement::isStatement}?*/ seq_of_statements | expression)
    ;

atom
    : table_element outer_join_sign
    | bind_variable
    | constant
    | general_element
    | LEFT_PAREN subquery RIGHT_PAREN subquery_operation_part*
    | LEFT_PAREN expressions RIGHT_PAREN
    ;

quantified_expression
    : (SOME | EXISTS | ALL | ANY) (LEFT_PAREN subquery RIGHT_PAREN | LEFT_PAREN expression RIGHT_PAREN)
    ;

string_function
    : SUBSTR LEFT_PAREN expression COMMA expression (COMMA expression)? RIGHT_PAREN
    | (LEFT | RIGHT) LEFT_PAREN expression COMMA expression RIGHT_PAREN
    | format_function
    | TO_CHAR LEFT_PAREN (table_element | standard_function | expression)
      (COMMA quoted_string)? (COMMA quoted_string)? RIGHT_PAREN
    | decode_function
    | CHR LEFT_PAREN concatenation USING NCHAR_CS RIGHT_PAREN
    | NVL LEFT_PAREN expression COMMA expression RIGHT_PAREN
    | trim_function
    | TO_DATE LEFT_PAREN expression (COMMA quoted_string)? RIGHT_PAREN
    ;

standard_function
    : string_function
    | numeric_function_wrapper
    | other_function
    | confidence_interval_function
    | raw_sql
    ;

literal
    : CHAR_STRING
    | string_function
    | numeric
    | MAXVALUE
    ;

numeric_function_wrapper
    : numeric_function (single_column_for_loop | multi_column_for_loop)?
    ;

numeric_function
   : SUM LEFT_PAREN (DISTINCT | ALL)? expression RIGHT_PAREN
   | COUNT LEFT_PAREN ( ASTERISK | ((DISTINCT | UNIQUE | ALL)? concatenation)? ) RIGHT_PAREN over_clause?
   | ROUND LEFT_PAREN expression (COMMA UNSIGNED_INTEGER)?  RIGHT_PAREN
   | AVG LEFT_PAREN (DISTINCT | ALL)? expression RIGHT_PAREN
   | MAX LEFT_PAREN (DISTINCT | ALL)? expression RIGHT_PAREN
   | MIN LEFT_PAREN (DISTINCT | ALL)? expression RIGHT_PAREN
   | median_function | log_function | ceil_function | floor_function
   | rank_function | stddev_function | variance_function
   | LEAST LEFT_PAREN expressions RIGHT_PAREN
   | GREATEST LEFT_PAREN expressions RIGHT_PAREN
   | POWER LEFT_PAREN concatenation COMMA concatenation RIGHT_PAREN
   | SQRT LEFT_PAREN concatenation RIGHT_PAREN
   ;

other_function
    : over_clause_keyword function_argument_analytic over_clause?
    | /*TODO stantard_function_enabling_using*/ regular_id function_argument_modeling using_clause?
    | COUNT LEFT_PAREN ( ASTERISK | (DISTINCT | UNIQUE | ALL)? concatenation) RIGHT_PAREN over_clause?
    | (CAST | XMLCAST) LEFT_PAREN (MULTISET LEFT_PAREN subquery RIGHT_PAREN | concatenation) AS type_spec RIGHT_PAREN
    | COALESCE LEFT_PAREN table_element (COMMA (numeric | quoted_string))? RIGHT_PAREN
    | COLLECT LEFT_PAREN (DISTINCT | UNIQUE)? concatenation collect_order_by_part? RIGHT_PAREN
    | percentile_function | coalesce_function
    | within_or_over_clause_keyword function_argument within_or_over_part+
    | cursor_name ( PERCENT_ISOPEN | PERCENT_FOUND | PERCENT_NOTFOUND | PERCENT_ROWCOUNT )
    | DECOMPOSE LEFT_PAREN concatenation (CANONICAL | COMPATIBILITY)? RIGHT_PAREN
    | extract_function | dates_function
    | EXTRACT LEFT_PAREN regular_id FROM concatenation RIGHT_PAREN
    | (FIRST_VALUE | LAST_VALUE) function_argument_analytic respect_or_ignore_nulls? over_clause
    | standard_prediction_function_keyword
      LEFT_PAREN expressions cost_matrix_clause? using_clause? RIGHT_PAREN
    | TRANSLATE LEFT_PAREN expression (USING (CHAR_CS | NCHAR_CS))? (COMMA expression)* RIGHT_PAREN
    | TREAT LEFT_PAREN expression AS REF? type_spec RIGHT_PAREN
    | TRIM LEFT_PAREN ((LEADING | TRAILING | BOTH)? quoted_string? FROM)? concatenation RIGHT_PAREN
    | XMLAGG LEFT_PAREN expression order_by_clause? RIGHT_PAREN (PERIOD general_element_part)?
    | (XMLCOLATTVAL | XMLFOREST)
      LEFT_PAREN (COMMA? xml_multiuse_expression_element)+ RIGHT_PAREN (PERIOD general_element_part)?
    | XMLELEMENT
      LEFT_PAREN (ENTITYESCAPING | NOENTITYESCAPING)? (NAME | EVALNAME)? expression
      (/*TODO{input.LT(2).getText().equalsIgnoreCase("xmlattributes")}?*/ COMMA xml_attributes_clause)?
      (COMMA expression column_alias?)* RIGHT_PAREN (PERIOD general_element_part)?
    | XMLEXISTS LEFT_PAREN expression xml_passing_clause? RIGHT_PAREN
    | XMLPARSE LEFT_PAREN (DOCUMENT | CONTENT) concatenation WELLFORMED? RIGHT_PAREN (PERIOD general_element_part)?
    | XMLPI
      LEFT_PAREN (NAME identifier | EVALNAME concatenation) (COMMA concatenation)? RIGHT_PAREN (PERIOD general_element_part)?
    | XMLQUERY
      LEFT_PAREN concatenation xml_passing_clause? RETURNING CONTENT (NULL ON EMPTY)? RIGHT_PAREN (PERIOD general_element_part)?
    | XMLROOT
      LEFT_PAREN concatenation (COMMA xmlroot_param_version_part)? (COMMA xmlroot_param_standalone_part)? RIGHT_PAREN (PERIOD general_element_part)?
    | XMLSERIALIZE
      LEFT_PAREN (DOCUMENT | CONTENT) concatenation (AS type_spec)?
      xmlserialize_param_enconding_part? xmlserialize_param_version_part? xmlserialize_param_ident_part? ((HIDE | SHOW) DEFAULTS)? RIGHT_PAREN
      (PERIOD general_element_part)?
    | XMLTABLE
      LEFT_PAREN xml_namespaces_clause? concatenation xml_passing_clause? (COLUMNS xml_table_column (COMMA xml_table_column))? RIGHT_PAREN (PERIOD general_element_part)?
    ;

over_clause_keyword
    : AVG
    | CORR
    | LAG
    | LEAD
    | MAX
    | MEDIAN
    | MIN
    | NTILE
    | RATIO_TO_REPORT
    | ROW_NUMBER
    | SUM
    | VARIANCE
    | REGR_
    | STDDEV
    | VAR_
    | COVAR_
    ;

within_or_over_clause_keyword
    : CUME_DIST
    | DENSE_RANK
    | LISTAGG
    | PERCENT_RANK
    | PERCENTILE_CONT
    | PERCENTILE_DISC
    | RANK
    ;

standard_prediction_function_keyword
    : PREDICTION
    | PREDICTION_BOUNDS
    | PREDICTION_COST
    | PREDICTION_DETAILS
    | PREDICTION_PROBABILITY
    | PREDICTION_SET
    ;

over_clause
    : OVER LEFT_PAREN query_partition_clause? (order_by_clause windowing_clause?)? RIGHT_PAREN
    ;

windowing_clause
    : windowing_type
      (BETWEEN windowing_elements AND windowing_elements | windowing_elements)
    ;

windowing_type
    : ROWS
    | RANGE
    ;

windowing_elements
    : UNBOUNDED PRECEDING
    | CURRENT ROW
    | concatenation (PRECEDING | FOLLOWING)
    ;

using_clause
    : USING (ASTERISK | (COMMA? using_element)+)
    ;

using_element
    : (IN OUT? | OUT)? select_list_elements column_alias?
    ;

collect_order_by_part
    : ORDER BY concatenation
    ;

within_or_over_part
    : WITHIN GROUP LEFT_PAREN order_by_clause RIGHT_PAREN
    | over_clause
    ;

cost_matrix_clause
    : COST (MODEL AUTO? | LEFT_PAREN (COMMA? cost_class_name)+ RIGHT_PAREN VALUES LEFT_PAREN expressions? RIGHT_PAREN)
    ;

xml_passing_clause
    : PASSING (BY VALUE)? expression column_alias? (COMMA expression column_alias?)
    ;

xml_attributes_clause
    : XMLATTRIBUTES
      LEFT_PAREN (ENTITYESCAPING | NOENTITYESCAPING)? (SCHEMACHECK | NOSCHEMACHECK)?
      (COMMA? xml_multiuse_expression_element)+ RIGHT_PAREN
    ;

xml_namespaces_clause
    : XMLNAMESPACES
      LEFT_PAREN (concatenation column_alias)? (COMMA concatenation column_alias)*
      xml_general_default_part? RIGHT_PAREN
    ;

xml_table_column
    : xml_column_name
      (FOR ORDINALITY | type_spec (PATH concatenation)? xml_general_default_part?)
    ;

xml_general_default_part
    : DEFAULT concatenation
    ;

xml_multiuse_expression_element
    : expression (AS (id_expression | EVALNAME concatenation))?
    ;

xmlroot_param_version_part
    : VERSION (NO VALUE | expression)
    ;

xmlroot_param_standalone_part
    : STANDALONE (YES | NO VALUE?)
    ;

xmlserialize_param_enconding_part
    : ENCODING concatenation
    ;

xmlserialize_param_version_part
    : VERSION concatenation
    ;

xmlserialize_param_ident_part
    : NO INDENT
    | INDENT (SIZE EQUALS_OP concatenation)?
    ;

// Common

partition_extension_clause
    : (SUBPARTITION | PARTITION) FOR? LEFT_PAREN expressions? RIGHT_PAREN
    ;

column_alias
    : AS? (identifier | quoted_string)
    | AS
    ;

table_alias
    : identifier
    | quoted_string
    ;

where_clause
    : WHERE (CURRENT OF cursor_name | expression)
    ;

into_clause
    : (BULK COLLECT)? INTO (COMMA? variable_name)+
    ;

// Common Named Elements

xml_column_name
    : identifier
    | quoted_string
    ;

cost_class_name
    : identifier
    ;

attribute_name
    : identifier
    ;

savepoint_name
    : identifier
    ;

rollback_segment_name
    : identifier
    ;

routine_name
    : identifier (PERIOD id_expression)* (AT_SIGN link_name)?
    ;

package_name
    : identifier
    ;

implementation_type_name
    : identifier (PERIOD id_expression)?
    ;

parameter_name
    : identifier
    ;

reference_model_name
    : identifier
    ;

main_model_name
    : identifier
    ;

aggregate_function_name
    : identifier (PERIOD id_expression)*
    ;

query_name
    : identifier
    ;

grantee_name
    : id_expression identified_by?
    ;

role_name
    : id_expression
    | CONNECT
    ;

constraint_name
    : identifier (PERIOD id_expression)* (AT_SIGN link_name)?
    ;

label_name
    : id_expression
    ;

type_name
    : id_expression (PERIOD id_expression)*
    ;

exception_name
    : identifier (PERIOD id_expression)*
    ;

function_name
    : identifier (PERIOD id_expression)?
    ;

variable_name
    : (INTRODUCER char_set_name)? id_expression (PERIOD id_expression)?
    | bind_variable
    ;

index_name
    : identifier (PERIOD id_expression)?
    ;

cursor_name
    : general_element
    | bind_variable
    ;

record_name
    : identifier
    | bind_variable
    ;

collection_name
    : identifier (PERIOD id_expression)?
    ;

link_name
    : identifier
    ;

column_name
    : identifier (PERIOD id_expression)*
    ;

tableview_name
    : identifier (PERIOD id_expression)?
      (AT_SIGN link_name | /*TODO{!(input.LA(2) == BY)}?*/ partition_extension_clause)?
    ;

char_set_name
    : id_expression (PERIOD id_expression)*
    ;

// Represents a valid DB object name in DDL commands which are valid for several DB (or schema) objects.
// For instance, create synonym ... for <DB object name>, or rename <old DB object name> to <new DB object name>.
// Both are valid for sequences, tables, views, etc.
schema_object_name
    : id_expression
    ;

dir_object_name
    : id_expression
    ;

user_object_name
    : id_expression
    ;

grant_object_name
    : tableview_name
    | USER (COMMA? user_object_name)+
    | DIRECTORY dir_object_name
    | EDITION schema_object_name
    | MINING MODEL schema_object_name
    | JAVA (SOURCE | RESOURCE) schema_object_name
    | SQL TRANSLATION PROFILE schema_object_name
    ;

column_list
    : (COMMA? column_name)+
    ;

paren_column_list
    : LEFT_PAREN column_list RIGHT_PAREN
    ;

// SQL Specs

// NOTE: In reality this applies to aggregate functions only
keep_clause
    : KEEP LEFT_PAREN DENSE_RANK (FIRST | LAST) order_by_clause RIGHT_PAREN over_clause?
    ;

function_argument
    : LEFT_PAREN (COMMA? argument)* RIGHT_PAREN keep_clause?
    ;

function_argument_analytic
    : LEFT_PAREN (COMMA? argument respect_or_ignore_nulls?)* RIGHT_PAREN keep_clause?
    ;

function_argument_modeling
    : LEFT_PAREN column_name (COMMA (numeric | NULL) (COMMA (numeric | NULL))?)?
      USING (tableview_name PERIOD ASTERISK | ASTERISK | (COMMA? expression column_alias?)+)
      RIGHT_PAREN keep_clause?
    ;

respect_or_ignore_nulls
    : (RESPECT | IGNORE) NULLS
    ;

argument
    : (identifier EQUALS_OP GREATER_THAN_OP)? expression
    ;

type_spec
    : datatype
    | REF? type_name (PERCENT_ROWTYPE | PERCENT_TYPE)?
    ;

datatype
    : native_datatype_element precision_part? (WITH LOCAL? TIME ZONE | CHARACTER SET char_set_name)?
    | INTERVAL (YEAR | DAY) (LEFT_PAREN expression RIGHT_PAREN)? TO (MONTH | SECOND) (LEFT_PAREN expression RIGHT_PAREN)?
    ;

precision_part
    : LEFT_PAREN numeric (COMMA numeric)? (CHAR | BYTE)? RIGHT_PAREN
    ;

native_datatype_element
    : BINARY_INTEGER
    | PLS_INTEGER
    | NATURAL
    | BINARY_FLOAT
    | BINARY_DOUBLE
    | NATURALN
    | POSITIVE
    | POSITIVEN
    | SIGNTYPE
    | SIMPLE_INTEGER
    | NVARCHAR2
    | DEC
    | INTEGER
    | INT
    | NUMERIC
    | SMALLINT
    | BIGINT
    | NUMBER
    | DECIMAL
    | DOUBLE PRECISION?
    | FLOAT
    | REAL
    | NCHAR
    | LONG RAW?
    | CHAR
    | CHARACTER
    | VARCHAR2
    | VARCHAR
    | STRING
    | RAW
    | BOOLEAN
    | DATE
    | ROWID
    | UROWID
    | YEAR
    | MONTH
    | DAY
    | HOUR
    | MINUTE
    | SECOND
    | TEXT
    | TIMEZONE_HOUR
    | TIMEZONE_MINUTE
    | TIMEZONE_REGION
    | TIMEZONE_ABBR
    | TIMESTAMP
    | TIMESTAMP_UNCONSTRAINED
    | TIMESTAMP_TZ_UNCONSTRAINED
    | TIMESTAMP_LTZ_UNCONSTRAINED
    | YMINTERVAL_UNCONSTRAINED
    | DSINTERVAL_UNCONSTRAINED
    | BFILE
    | BLOB
    | CLOB
    | NCLOB
    | MLSLABEL
    | GUID
    | UUID
    ;

bind_variable
    : (BINDVAR | COLON UNSIGNED_INTEGER)
      // Pro*C/C++ indicator variables
      (INDICATOR? (BINDVAR | COLON UNSIGNED_INTEGER))?
      (PERIOD general_element_part)*
    ;

general_element
    : general_element_part (PERIOD general_element_part)*
    ;

general_element_part
    : (INTRODUCER char_set_name)? id_expression (PERIOD id_expression)* (AT_SIGN link_name)? function_argument?
    ;

table_element
    : (INTRODUCER char_set_name)? id_expression (PERIOD id_expression)*
    ;

object_privilege
    : ALL PRIVILEGES?
    | ALTER
    | DEBUG
    | DELETE
    | EXECUTE
    | FLASHBACK ARCHIVE
    | INDEX
    | INHERIT PRIVILEGES
    | INSERT
    | KEEP SEQUENCE
    | MERGE VIEW
    | ON COMMIT REFRESH
    | QUERY REWRITE
    | READ
    | REFERENCES
    | SELECT
    | TRANSLATE SQL
    | UNDER
    | UPDATE
    | USE
    | WRITE
    ;

//Ordered by type rather than alphabetically
system_privilege
    : ALL PRIVILEGES
    | ADVISOR
    | ADMINISTER ANY? SQL TUNING SET
    | (ALTER | CREATE | DROP) ANY SQL PROFILE
    | ADMINISTER SQL MANAGEMENT OBJECT
    | CREATE ANY? CLUSTER
    | (ALTER | DROP) ANY CLUSTER
    | (CREATE | DROP) ANY CONTEXT
    | EXEMPT REDACTION POLICY
    | ALTER DATABASE
    | (ALTER | CREATE) PUBLIC? DATABASE LINK
    | DROP PUBLIC DATABASE LINK
    | DEBUG CONNECT SESSION
    | DEBUG ANY PROCEDURE
    | ANALYZE ANY DICTIONARY
    | CREATE ANY? DIMENSION
    | (ALTER | DROP) ANY DIMENSION
    | (CREATE | DROP) ANY DIRECTORY
    | (CREATE | DROP) ANY EDITION
    | FLASHBACK (ARCHIVE ADMINISTER | ANY TABLE)
    | (ALTER | CREATE | DROP) ANY INDEX
    | CREATE ANY? INDEXTYPE
    | (ALTER | DROP | EXECUTE) ANY INDEXTYPE
    | CREATE (ANY | EXTERNAL)? JOB
    | EXECUTE ANY (CLASS | PROGRAM)
    | MANAGE SCHEDULER
    | ADMINISTER KEY MANAGEMENT
    | CREATE ANY? LIBRARY
    | (ALTER | DROP | EXECUTE) ANY LIBRARY
    | LOGMINING
    | CREATE ANY? MATERIALIZED VIEW
    | (ALTER | DROP) ANY MATERIALIZED VIEW
    | GLOBAL? QUERY REWRITE
    | ON COMMIT REFRESH
    | CREATE ANY? MINING MODEL
    | (ALTER | DROP | SELECT | COMMENT) ANY MINING MODEL
    | CREATE ANY? CUBE
    | (ALTER | DROP | SELECT | UPDATE) ANY CUBE
    | CREATE ANY? MEASURE FOLDER
    | (DELETE | DROP | INSERT) ANY MEASURE FOLDER
    | CREATE ANY? CUBE DIMENSION
    | (ALTER | DELETE | DROP | INSERT | SELECT | UPDATE) ANY CUBE DIMENSION
    | CREATE ANY? CUBE BUILD PROCESS
    | (DROP | UPDATE) ANY CUBE BUILD PROCESS
    | CREATE ANY? OPERATOR
    | (ALTER | DROP | EXECUTE) ANY OPERATOR
    | (CREATE | ALTER | DROP) ANY OUTLINE
    | CREATE PLUGGABLE DATABASE
    | SET CONTAINER
    | CREATE ANY? PROCEDURE
    | (ALTER | DROP | EXECUTE) ANY PROCEDURE
    | (CREATE | ALTER | DROP ) PROFILE
    | CREATE ROLE
    | (ALTER | DROP | GRANT) ANY ROLE
    | (CREATE | ALTER | DROP) ROLLBACK SEGMENT
    | CREATE ANY? SEQUENCE
    | (ALTER | DROP | SELECT) ANY SEQUENCE
    | (ALTER | CREATE | RESTRICTED) SESSION
    | ALTER RESOURCE COST
    | CREATE ANY? SQL TRANSLATION PROFILE
    | (ALTER | DROP | USE) ANY SQL TRANSLATION PROFILE
    | TRANSLATE ANY SQL
    | CREATE ANY? SYNONYM
    | DROP ANY SYNONYM
    | (CREATE | DROP) PUBLIC SYNONYM
    | CREATE ANY? TABLE
    | (ALTER | BACKUP | COMMENT | DELETE | DROP | INSERT | LOCK | READ | SELECT | UPDATE) ANY TABLE
    | (CREATE | ALTER | DROP | MANAGE | UNLIMITED) TABLESPACE
    | CREATE ANY? TRIGGER
    | (ALTER | DROP) ANY TRIGGER
    | ADMINISTER DATABASE TRIGGER
    | CREATE ANY? TYPE
    | (ALTER | DROP | EXECUTE | UNDER) ANY TYPE
    | (CREATE | ALTER | DROP) USER
    | CREATE ANY? VIEW
    | (DROP | UNDER | MERGE) ANY VIEW
    | (ANALYZE | AUDIT) ANY
    | BECOME USER
    | CHANGE NOTIFICATION
    | EXEMPT ACCESS POLICY
    | FORCE ANY? TRANSACTION
    | GRANT ANY OBJECT? PRIVILEGE
    | INHERIT ANY PRIVILEGES
    | KEEP DATE TIME
    | KEEP SYSGUID
    | PURGE DBA_RECYCLEBIN
    | RESUMABLE
    | SELECT ANY (DICTIONARY | TRANSACTION)
    | SYSBACKUP
    | SYSDBA
    | SYSDG
    | SYSKM
    | SYSOPER
    ;

// $>

// $<Lexer Mappings

constant
    : TIMESTAMP (quoted_string | bind_variable) (AT TIME ZONE quoted_string)?
    | INTERVAL (quoted_string | bind_variable | general_element_part)
      (YEAR | MONTH | DAY | HOUR | MINUTE | SECOND)?
      (LEFT_PAREN (UNSIGNED_INTEGER | bind_variable) (COMMA (UNSIGNED_INTEGER | bind_variable) )? RIGHT_PAREN)?
      (TO ( DAY | HOUR | MINUTE | SECOND (LEFT_PAREN (UNSIGNED_INTEGER | bind_variable) RIGHT_PAREN)?))?
    | numeric
    | DATE quoted_string
    | quoted_string
    | DATE_TIME
    | NULL
    | TRUE
    | FALSE
    | DBTIMEZONE
    | SESSIONTIMEZONE
    | MINVALUE
    | MAXVALUE
    | DEFAULT
    ;

numeric
    : UNSIGNED_INTEGER
    | APPROXIMATE_NUM_LIT
    ;

numeric_negative
    : MINUS_SIGN numeric
    ;

quoted_string
    : CHAR_STRING
    // | CHAR_STRING_PERL
    | NATIONAL_CHAR_STRING_LIT
    ;

identifier
    : (INTRODUCER char_set_name)? id_expression
    ;

id_expression
    : special_property
    | special_id
    | regular_id
    | DELIMITED_ID
    ;

outer_join_sign
    : LEFT_PAREN PLUS_SIGN RIGHT_PAREN
    ;

regular_id
    : non_reserved_keywords_pre12c
    | non_reserved_keywords_in_12c
    | PUBLIC
    | REGULAR_ID
    | A_LETTER
    | AGENT
    | AGGREGATE
    | ANALYZE
    | AUTONOMOUS_TRANSACTION
    | BATCH
    | BINARY_INTEGER
    | BOOLEAN
    | C_LETTER
    | CHAR
    | CLUSTER
    | CONSTRUCTOR
    | CUSTOMDATUM
    | DECIMAL
    | DELETE
    | DETERMINISTIC
    | DSINTERVAL_UNCONSTRAINED
    | ERR
    | EXCEPTION
    | EXCEPTION_INIT
    | EXCEPTIONS
    | EXIT
    | FLOAT
    | FORALL
    | INDICES
    | INOUT
    | INTEGER
    | LANGUAGE
    | LONG
    | LOOP
    | NUMBER
    | ORADATA
    | OSERROR
    | OUT
    | OVERRIDING
    | PARALLEL_ENABLE
    | PIPELINED
    | PLS_INTEGER
    | POSITIVE
    | POSITIVEN
    | PRAGMA
    | RAISE
    | RAW
    | RECORD
    | REF
    | RENAME
    | RESTRICT_REFERENCES
    | RESULT
    | SELF
    | SERIALLY_REUSABLE
    | SET
    | SIGNTYPE
    | SIMPLE_INTEGER
    | SMALLINT
    | SQLDATA
    | SQLERROR
    | SUBTYPE
    | TIMESTAMP_LTZ_UNCONSTRAINED
    | TIMESTAMP_TZ_UNCONSTRAINED
    | TIMESTAMP_UNCONSTRAINED
    | TRIGGER
    | VARCHAR
    | VARCHAR2
    | VARIABLE
    | WARNING
    | WHILE
    | XMLAGG
    | YMINTERVAL_UNCONSTRAINED
    | REGR_
    | VAR_
    | COVAR_
    ;

non_reserved_keywords_in_12c
    : ACL
    | ACTION
    | ACTIONS
    | ACTIVE
    | ACTIVE_DATA
    | ACTIVITY
    | ADAPTIVE_PLAN
    | ADVANCED
    | AFD_DISKSTRING
    | ANOMALY
    | ANSI_REARCH
    | APPLICATION
    | APPROX_COUNT_DISTINCT
    | ARCHIVAL
    | ARCHIVED
    | ASIS
    | ASSIGN
    | AUTO_LOGIN
    | AUTO_REOPTIMIZE
    | AVRO
    | BACKGROUND
    | BATCHSIZE
    | BATCH_TABLE_ACCESS_BY_ROWID
    | BEGINNING
    | BEQUEATH
    | BITMAP_AND
    | BSON
    | CACHING
    | CALCULATED
    | CALLBACK
    | CAPACITY
    | CDBDEFAULT
    | CLASSIFIER
    | CLEANUP
    | CLIENT
    | CLUSTER_DETAILS
    | CLUSTER_DISTANCE
    | CLUSTERING
    | COMMON_DATA
    | COMPONENT
    | COMPONENTS
    | CON_DBID_TO_ID
    | CONDITION
    | CONDITIONAL
    | CON_GUID_TO_ID
    | CON_ID
    | CON_NAME_TO_ID
    | CONTAINER_DATA
    | CONTAINERS
    | CON_UID_TO_ID
    | COOKIE
    | COPY
    | CREATE_FILE_DEST
    | CREDENTIAL
    | CRITICAL
    | CUBE_AJ
    | CUBE_SJ
    | DATAMOVEMENT
    | DATAOBJ_TO_MAT_PARTITION
    | DATAPUMP
    | DATA_SECURITY_REWRITE_LIMIT
    | DAYS
    | DB_UNIQUE_NAME
    | DECORRELATE
    | DEFINE
    | DELEGATE
    | DELETE_ALL
    | DESTROY
    | DIMENSIONS
    | DISABLE_ALL
    | DISABLE_PARALLEL_DML
    | DISCARD
    | DISTRIBUTE
    | DUPLICATE
    | DV
    | EDITIONABLE
    | ELIM_GROUPBY
    | EM
    | ENABLE_ALL
    | ENABLE_PARALLEL_DML
    | EQUIPART
    | EVAL
    | EVALUATE
    | EXISTING
    | EXPRESS
    | EXTRACTCLOBXML
    | FACTOR
    | FAILOVER
    | FAILURE
    | FAMILY
    | FAR
    | FASTSTART
    | FEATURE_DETAILS
    | FETCH
    | FILE_NAME_CONVERT
    | FIXED_VIEW_DATA
    | FORMAT
    | GATHER_OPTIMIZER_STATISTICS
    | GET
    | ILM
    | INACTIVE
    | INDEXING
    | INHERIT
    | INMEMORY
    | INMEMORY_PRUNING
    | INPLACE
    | INTERLEAVED
    | JSON
    | JSON_ARRAY
    | JSON_ARRAYAGG
    | JSON_EQUAL
    | JSON_EXISTS
    | JSON_EXISTS2
    | JSONGET
    | JSON_OBJECT
    | JSON_OBJECTAGG
    | JSONPARSE
    | JSON_QUERY
    | JSON_SERIALIZE
    | JSON_TABLE
    | JSON_TEXTCONTAINS
    | JSON_TEXTCONTAINS2
    | JSON_VALUE
    | KEYSTORE
    | LABEL
    | LAX
    | LIFECYCLE
    | LINEAR
    | LOCKING
    | LOGMINING
    | MAP
    | MATCH
    | MATCHES
    | MATCH_NUMBER
    | MATCH_RECOGNIZE
    | MAX_SHARED_TEMP_SIZE
    | MEMCOMPRESS
    | METADATA
    | MODEL_NB
    | MODEL_SV
    | MODIFICATION
    | MODULE
    | MONTHS
    | MULTIDIMENSIONAL
    | NEG
    | NO_ADAPTIVE_PLAN
    | NO_ANSI_REARCH
    | NO_AUTO_REOPTIMIZE
    | NO_BATCH_TABLE_ACCESS_BY_ROWID
    | NO_CLUSTERING
    | NO_COMMON_DATA
    | NOCOPY
    | NO_DATA_SECURITY_REWRITE
    | NO_DECORRELATE
    | NO_ELIM_GROUPBY
    | NO_GATHER_OPTIMIZER_STATISTICS
    | NO_INMEMORY
    | NO_INMEMORY_PRUNING
    | NOKEEP
    | NONEDITIONABLE
    | NO_OBJECT_LINK
    | NO_PARTIAL_JOIN
    | NO_PARTIAL_ROLLUP_PUSHDOWN
    | NOPARTITION
    | NO_PQ_CONCURRENT_UNION
    | NO_PQ_REPLICATE
    | NO_PQ_SKEW
    | NO_PX_FAULT_TOLERANCE
    | NORELOCATE
    | NOREPLAY
    | NO_ROOT_SW_FOR_LOCAL
    | NO_SQL_TRANSLATION
    | NO_USE_CUBE
    | NO_USE_VECTOR_AGGREGATION
    | NO_VECTOR_TRANSFORM
    | NO_VECTOR_TRANSFORM_DIMS
    | NO_VECTOR_TRANSFORM_FACT
    | NO_ZONEMAP
    | OBJ_ID
    | OFFSET
    | OLS
    | OMIT
    | ONE
    | ORA_CHECK_ACL
    | ORA_CHECK_PRIVILEGE
    | ORA_CLUSTERING
    | ORA_INVOKING_USER
    | ORA_INVOKING_USERID
    | ORA_INVOKING_XS_USER
    | ORA_INVOKING_XS_USER_GUID
    | ORA_RAWCOMPARE
    | ORA_RAWCONCAT
    | ORA_WRITE_TIME
    | PARTIAL
    | PARTIAL_JOIN
    | PARTIAL_ROLLUP_PUSHDOWN
    | PAST
    | PATCH
    | PATH_PREFIX
    | PATTERN
    | PER
    | PERIOD
    | PERMUTE
    | PLUGGABLE
    | POOL_16K
    | POOL_2K
    | POOL_32K
    | POOL_4K
    | POOL_8K
    | PQ_CONCURRENT_UNION
    | PQ_DISTRIBUTE_WINDOW
    | PQ_FILTER
    | PQ_REPLICATE
    | PQ_SKEW
    | PRELOAD
    | PRETTY
    | PREV
    | PRINTBLOBTOCLOB
    | PRIORITY
    | PRIVILEGED
    | PROXY
    | PRUNING
    | PX_FAULT_TOLERANCE
    | REALM
    | REDEFINE
    | RELOCATE
    | RESTART
    | ROLESET
    | ROWID_MAPPING_TABLE
    | RUNNING
    | SAVE
    | SCRUB
    | SDO_GEOM_MBR
    | SECRET
    | SERIAL
    | SERVICE_NAME_CONVERT
    | SERVICES
    | SHARING
    | SHELFLIFE
    | SOURCE_FILE_DIRECTORY
    | SOURCE_FILE_NAME_CONVERT
    | SQL_TRANSLATION_PROFILE
    | STANDARD_HASH
    | STANDBYS
    | STATE
    | STATEMENT
    | STREAM
    | SUBSCRIBE
    | SUBSET
    | SUCCESS
    | SYSBACKUP
    | SYS_CHECK_PRIVILEGE
    | SYSDG
    | SYS_GET_COL_ACLIDS
    | SYSGUID
    | SYSKM
    | SYS_MKXTI
    | SYSOBJ
    | SYS_OP_CYCLED_SEQ
    | SYS_OP_HASH
    | SYS_OP_KEY_VECTOR_CREATE
    | SYS_OP_KEY_VECTOR_FILTER
    | SYS_OP_KEY_VECTOR_FILTER_LIST
    | SYS_OP_KEY_VECTOR_SUCCEEDED
    | SYS_OP_KEY_VECTOR_USE
    | SYS_OP_PART_ID
    | SYS_OP_ZONE_ID
    | SYS_RAW_TO_XSID
    | SYS_XSID_TO_RAW
    | SYS_ZMAP_FILTER
    | SYS_ZMAP_REFRESH
    | TAG
    | TEXT
    | TIER
    | TIES
    | TO_ACLID
    | TRANSLATION
    | TRUST
    | UCS2
    | UNCONDITIONAL
    | UNMATCHED
    | UNPLUG
    | UNSUBSCRIBE
    | USABLE
    | USE_CUBE
    | USE_HIDDEN_PARTITIONS
    | USER_DATA
    | USER_TABLESPACES
    | USE_VECTOR_AGGREGATION
    | USING_NO_EXPAND
    | UTF16BE
    | UTF16LE
    | UTF32
    | UTF8
    | V1
    | V2
    | VALID_TIME_END
    | VECTOR_TRANSFORM
    | VECTOR_TRANSFORM_DIMS
    | VECTOR_TRANSFORM_FACT
    | VERIFIER
    | VIOLATION
    | VISIBILITY
    | WEEK
    | WEEKS
    | WITH_PLSQL
    | WRAPPER
    | XS
    | YEARS
    | ZONEMAP
    ;

non_reserved_keywords_pre12c
    : ABORT
    | ABS
    | ACCESSED
    | ACCESS
    | ACCOUNT
    | ACOS
    | ACTIVATE
    | ACTIVE_COMPONENT
    | ACTIVE_FUNCTION
    | ACTIVE_TAG
    | ADD_COLUMN
    | ADD_GROUP
    | ADD_MONTHS
    | ADD
    | ADJ_DATE
    | ADMINISTER
    | ADMINISTRATOR
    | ADMIN
    | ADVISE
    | ADVISOR
    | AFTER
    | ALIAS
    | ALLOCATE
    | ALLOW
    | ALL_ROWS
    | ALWAYS
    | ANALYZE
    | ANCILLARY
    | AND_EQUAL
    | ANTIJOIN
    | ANYSCHEMA
    | APPENDCHILDXML
    | APPEND
    | APPEND_VALUES
    | APPLY
    | ARCHIVELOG
    | ARCHIVE
    | ARRAY
    | ASCII
    | ASCIISTR
    | ASIN
    | ASSEMBLY
    | ASSOCIATE
    | ASYNCHRONOUS
    | ASYNC
    | ATAN2
    | ATAN
    | AT
    | ATTRIBUTE
    | ATTRIBUTES
    | AUTHENTICATED
    | AUTHENTICATION
    | AUTHID
    | AUTHORIZATION
    | AUTOALLOCATE
    | AUTOEXTEND
    | AUTOMATIC
    | AUTO
    | AVAILABILITY
    | AVG
    | BACKUP
    | BASICFILE
    | BASIC
    | BATCH
    | BECOME
    | BEFORE
    | BEGIN
    | BEGIN_OUTLINE_DATA
    | BEHALF
    | BFILE
    | BFILENAME
    | BIGFILE
    | BINARY_DOUBLE_INFINITY
    | BINARY_DOUBLE
    | BINARY_DOUBLE_NAN
    | BINARY_FLOAT_INFINITY
    | BINARY_FLOAT
    | BINARY_FLOAT_NAN
    | BINARY
    | BIND_AWARE
    | BINDING
    | BIN_TO_NUM
    | BITAND
    | BITMAP
    | BITMAPS
    | BITMAP_TREE
    | BITS
    | BLOB
    | BLOCK
    | BLOCK_RANGE
    | BLOCKSIZE
    | BLOCKS
    | BODY
    | BOTH
    | BOUND
    | BRANCH
    | BREADTH
    | BROADCAST
    | BUFFER_CACHE
    | BUFFER
    | BUFFER_POOL
    | BUILD
    | BULK
    | BYPASS_RECURSIVE_CHECK
    | BYPASS_UJVC
    | BYTE
    | CACHE_CB
    | CACHE_INSTANCES
    | CACHE
    | CACHE_TEMP_TABLE
    | CALL
    | CANCEL
    | CARDINALITY
    | CASCADE
    | CASE
    | CAST
    | CATEGORY
    | CEIL
    | CELL_FLASH_CACHE
    | CERTIFICATE
    | CFILE
    | CHAINED
    | CHANGE_DUPKEY_ERROR_INDEX
    | CHANGE
    | CHARACTER
    | CHAR_CS
    | CHARTOROWID
    | CHECK_ACL_REWRITE
    | CHECKPOINT
    | CHILD
    | CHOOSE
    | CHR
    | CHUNK
    | CLASS
    | CLEAR
    | CLOB
    | CLONE
    | CLOSE_CACHED_OPEN_CURSORS
    | CLOSE
    | CLUSTER_BY_ROWID
    | CLUSTER_ID
    | CLUSTERING_FACTOR
    | CLUSTER_PROBABILITY
    | CLUSTER_SET
    | COALESCE
    | COALESCE_SQ
    | COARSE
    | CO_AUTH_IND
    | COLD
    | COLLECT
    | COLUMNAR
    | COLUMN_AUTH_INDICATOR
    | COLUMN
    | COLUMNS
    | COLUMN_STATS
    | COLUMN_VALUE
    | COMMENT
    | COMMIT
    | COMMITTED
    | COMPACT
    | COMPATIBILITY
    | COMPILE
    | COMPLETE
    | COMPLIANCE
    | COMPOSE
    | COMPOSITE_LIMIT
    | COMPOSITE
    | COMPOUND
    | COMPUTE
    | CONCAT
    | CONFIRM
    | CONFORMING
    | CONNECT_BY_CB_WHR_ONLY
    | CONNECT_BY_COMBINE_SW
    | CONNECT_BY_COST_BASED
    | CONNECT_BY_ELIM_DUPS
    | CONNECT_BY_FILTERING
    | CONNECT_BY_ISCYCLE
    | CONNECT_BY_ISLEAF
    | CONNECT_BY_ROOT
    | CONNECT_TIME
    | CONSIDER
    | CONSISTENT
    | CONSTANT
    | CONST
    | CONSTRAINT
    | CONSTRAINTS
    | CONTAINER
    | CONTENT
    | CONTENTS
    | CONTEXT
    | CONTINUE
    | CONTROLFILE
    | CONVERT
    | CORR_K
    | CORR
    | CORR_S
    | CORRUPTION
    | CORRUPT_XID_ALL
    | CORRUPT_XID
    | COSH
    | COS
    | COST
    | COST_XML_QUERY_REWRITE
    | COUNT
    | COVAR_POP
    | COVAR_SAMP
    | CPU_COSTING
    | CPU_PER_CALL
    | CPU_PER_SESSION
    | CRASH
    | CREATE_STORED_OUTLINES
    | CREATION
    | CROSSEDITION
    | CROSS
    | CSCONVERT
    | CUBE_GB
    | CUBE
    | CUME_DISTM
    | CUME_DIST
    | CURRENT_DATE
    | CURRENT
    | CURRENT_SCHEMA
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | CURRENT_USER
    | CURRENTV
    | CURSOR
    | CURSOR_SHARING_EXACT
    | CURSOR_SPECIFIC_SEGMENT
    | CV
    | CYCLE
    | DANGLING
    | DATABASE
    | DATAFILE
    | DATAFILES
    | DATA
    | DATAOBJNO
    | DATAOBJ_TO_PARTITION
    | DATE_MODE
    | DAY
    | DBA
    | DBA_RECYCLEBIN
    | DBMS_STATS
    | DB_ROLE_CHANGE
    | DBTIMEZONE
    | DB_VERSION
    | DDL
    | DEALLOCATE
    | DEBUGGER
    | DEBUG
    | DECLARE
    | DEC
    | DECOMPOSE
    | DECREMENT
    | DECR
    | DECRYPT
    | DEDUPLICATE
    | DEFAULTS
    | DEFERRABLE
    | DEFERRED
    | DEFINED
    | DEFINER
    | DEGREE
    | DELAY
    | DELETEXML
    | DEMAND
    | DENSE_RANKM
    | DENSE_RANK
    | DEPENDENT
    | DEPTH
    | DEQUEUE
    | DEREF
    | DEREF_NO_REWRITE
    | DETACHED
    | DETERMINES
    | DICTIONARY
    | DIMENSION
    | DIRECT_LOAD
    | DIRECTORY
    | DIRECT_PATH
    | DISABLE
    | DISABLE_PRESET
    | DISABLE_RPKE
    | DISALLOW
    | DISASSOCIATE
    | DISCONNECT
    | DISKGROUP
    | DISK
    | DISKS
    | DISMOUNT
    | DISTINGUISHED
    | DISTRIBUTED
    | DML
    | DML_UPDATE
    | DOCFIDELITY
    | DOCUMENT
    | DOMAIN_INDEX_FILTER
    | DOMAIN_INDEX_NO_SORT
    | DOMAIN_INDEX_SORT
    | DOUBLE
    | DOWNGRADE
    | DRIVING_SITE
    | DROP_COLUMN
    | DROP_GROUP
    | DST_UPGRADE_INSERT_CONV
    | DUMP
    | DYNAMIC
    | DYNAMIC_SAMPLING_EST_CDN
    | DYNAMIC_SAMPLING
    | EACH
    | EDITIONING
    | EDITION
    | EDITIONS
    | ELEMENT
    | ELIMINATE_JOIN
    | ELIMINATE_OBY
    | ELIMINATE_OUTER_JOIN
    | EMPTY_BLOB
    | EMPTY_CLOB
    | EMPTY
    | ENABLE
    | ENABLE_PRESET
    | ENCODING
    | ENCRYPTION
    | ENCRYPT
    | END
    | END_OUTLINE_DATA
    | ENFORCED
    | ENFORCE
    | ENQUEUE
    | ENTERPRISE
    | ENTITYESCAPING
    | ENTRY
    | ERROR_ARGUMENT
    | ERROR
    | ERROR_ON_OVERLAP_TIME
    | ERRORS
    | ESCAPE
    | ESTIMATE
    | EVALNAME
    | EVALUATION
    | EVENTS
    | EVERY
    | EXCEPTIONS
    | EXCEPT
    | EXCHANGE
    | EXCLUDE
    | EXCLUDING
    | EXECUTE
    | EXEMPT
    | EXISTSNODE
    | EXPAND_GSET_TO_UNION
    | EXPAND_TABLE
    | EXPIRE
    | EXPLAIN
    | EXPLOSION
    | EXP
    | EXPORT
    | EXPR_CORR_CHECK
    | EXTENDS
    | EXTENT
    | EXTENTS
    | EXTERNALLY
    | EXTERNAL
    | EXTRACT
    | EXTRACTVALUE
    | EXTRA
    | FACILITY
    | FACT
    | FACTORIZE_JOIN
    | FAILED_LOGIN_ATTEMPTS
    | FAILED
    | FAILGROUP
    | FALSE
    | FAST
    | FBTSCAN
    | FEATURE_ID
    | FEATURE_SET
    | FEATURE_VALUE
    | FILE
    | FILESYSTEM_LIKE_LOGGING
    | FILTER
    | FINAL
    | FINE
    | FINISH
    | FIRSTM
    | FIRST
    | FIRST_ROWS
    | FIRST_VALUE
    | FLAGGER
    | FLASHBACK
    | FLASH_CACHE
    | FLOB
    | FLOOR
    | FLUSH
    | FOLDER
    | FOLLOWING
    | FOLLOWS
    | FORCE
    | FORCE_XML_QUERY_REWRITE
    | FOREIGN
    | FOREVER
    | FORWARD
    | FRAGMENT_NUMBER
    | FREELIST
    | FREELISTS
    | FREEPOOLS
    | FRESH
    | FROM_TZ
    | FULL
    | FULL_OUTER_JOIN_TO_OUTER
    | FUNCTION
    | FUNCTIONS
    | GATHER_PLAN_STATISTICS
    | GBY_CONC_ROLLUP
    | GBY_PUSHDOWN
    | GENERATED
    | GLOBALLY
    | GLOBAL
    | GLOBAL_NAME
    | GLOBAL_TOPIC_ENABLED
    | GREATEST
    | GROUP_BY
    | GROUP_ID
    | GROUPING_ID
    | GROUPING
    | GROUPS
    | GUARANTEED
    | GUARANTEE
    | GUARD
    | HASH_AJ
    | HASHKEYS
    | HASH
    | HASH_SJ
    | HEADER
    | HEAP
    | HELP
    | HEXTORAW
    | HEXTOREF
    | HIDDEN_KEYWORD
    | HIDE
    | HIERARCHY
    | HIGH
    | HINTSET_BEGIN
    | HINTSET_END
    | HOT
    | HOUR
    | HWM_BROKERED
    | HYBRID
    | IDENTIFIER
    | IDENTITY
    | IDGENERATORS
    | IDLE_TIME
    | ID
    | IF
    | IGNORE
    | IGNORE_OPTIM_EMBEDDED_HINTS
    | IGNORE_ROW_ON_DUPKEY_INDEX
    | IGNORE_WHERE_CLAUSE
    | IMMEDIATE
    | IMPACT
    | IMPORT
    | INCLUDE
    | INCLUDE_VERSION
    | INCLUDING
    | INCREMENTAL
    | INCREMENT
    | INCR
    | INDENT
    | INDEX_ASC
    | INDEX_COMBINE
    | INDEX_DESC
    | INDEXED
    | INDEXES
    | INDEX_FFS
    | INDEX_FILTER
    | INDEX_JOIN
    | INDEX_ROWS
    | INDEX_RRS
    | INDEX_RS_ASC
    | INDEX_RS_DESC
    | INDEX_RS
    | INDEX_SCAN
    | INDEX_SKIP_SCAN
    | INDEX_SS_ASC
    | INDEX_SS_DESC
    | INDEX_SS
    | INDEX_STATS
    | INDEXTYPE
    | INDEXTYPES
    | INDICATOR
    | INFINITE
    | INFORMATIONAL
    | INITCAP
    | INITIALIZED
    | INITIALLY
    | INITIAL
    | INITRANS
    | INLINE
    | INLINE_XMLTYPE_NT
    | IN_MEMORY_METADATA
    | INNER
    | INSERTCHILDXMLAFTER
    | INSERTCHILDXMLBEFORE
    | INSERTCHILDXML
    | INSERTXMLAFTER
    | INSERTXMLBEFORE
    | INSTANCE
    | INSTANCES
    | INSTANTIABLE
    | INSTANTLY
    | INSTEAD
    | INSTR2
    | INSTR4
    | INSTRB
    | INSTRC
    | INSTR
    | INTERMEDIATE
    | INTERNAL_CONVERT
    | INTERNAL_USE
    | INTERPRETED
    | INTERVAL
    | INT
    | INVALIDATE
    | INVISIBLE
    | IN_XQUERY
    | ISOLATION_LEVEL
    | ISOLATION
    | ITERATE
    | ITERATION_NUMBER
    | JAVA
    | JOB
    | JOIN
    | KEEP_DUPLICATES
    | KEEP
    | KERBEROS
    | KEY_LENGTH
    | KEY
    | KEYSIZE
    | KEYS
    | KILL
    | LAG
    | LAST_DAY
    | LAST
    | LAST_VALUE
    | LATERAL
    | LAYER
    | LDAP_REGISTRATION_ENABLED
    | LDAP_REGISTRATION
    | LDAP_REG_SYNC_INTERVAL
    | LEADING
    | LEAD
    | LEAST
    | LEFT
    | LENGTH2
    | LENGTH4
    | LENGTHB
    | LENGTHC
    | LENGTH
    | LESS
    | LEVEL
    | LEVELS
    | LIBRARY
    | LIFE
    | LIFETIME
    | LIKE2
    | LIKE4
    | LIKEC
    | LIKE_EXPAND
    | LIMIT
    | LINK
    | LISTAGG
    | LIST
    | LN
    | LNNVL
    | LOAD
    | LOB
    | LOBNVL
    | LOBS
    | LOCAL_INDEXES
    | LOCAL
    | LOCALTIME
    | LOCALTIMESTAMP
    | LOCATION
    | LOCATOR
    | LOCKED
    | LOGFILE
    | LOGFILES
    | LOGGING
    | LOGICAL
    | LOGICAL_READS_PER_CALL
    | LOGICAL_READS_PER_SESSION
    | LOG
    | LOGOFF
    | LOGON
    | LOG_READ_ONLY_VIOLATIONS
    | LOWER
    | LOW
    | LPAD
    | LTRIM
    | MAIN
    | MAKE_REF
    | MANAGED
    | MANAGEMENT
    | MANAGE
    | MANAGER
    | MANUAL
    | MAPPING
    | MASTER
    | MATCHED
    | MATERIALIZED
    | MATERIALIZE
    | MAXARCHLOGS
    | MAXDATAFILES
    | MAXEXTENTS
    | MAXIMIZE
    | MAXINSTANCES
    | MAXLOGFILES
    | MAXLOGHISTORY
    | MAXLOGMEMBERS
    | MAX
    | MAXSIZE
    | MAXTRANS
    | MAXVALUE
    | MEASURE
    | MEASURES
    | MEDIAN
    | MEDIUM
    | MEMBER
    | MEMORY
    | MERGEACTIONS
    | MERGE_AJ
    | MERGE_CONST_ON
    | MERGE
    | MERGE_SJ
    | METHOD
    | MIGRATE
    | MIGRATION
    | MINEXTENTS
    | MINIMIZE
    | MINIMUM
    | MINING
    | MIN
    | MINUS_NULL
    | MINUTE
    | MINVALUE
    | MIRRORCOLD
    | MIRRORHOT
    | MIRROR
    | MLSLABEL
    | MODEL_COMPILE_SUBQUERY
    | MODEL_DONTVERIFY_UNIQUENESS
    | MODEL_DYNAMIC_SUBQUERY
    | MODEL_MIN_ANALYSIS
    | MODEL
    | MODEL_NO_ANALYSIS
    | MODEL_PBY
    | MODEL_PUSH_REF
    | MODIFY_COLUMN_TYPE
    | MODIFY
    | MOD
    | MONITORING
    | MONITOR
    | MONTH
    | MONTHS_BETWEEN
    | MOUNT
    | MOUNTPATH
    | MOVEMENT
    | MOVE
    | MULTISET
    | MV_MERGE
    | NAMED
    | NAME
    | NAMESPACE
    | NAN_
    | NANVL
    | NATIONAL
    | NATIVE_FULL_OUTER_JOIN
    | NATIVE
    | NATURAL
    | NAV
    | NCHAR_CS
    | NCHAR
    | NCHR
    | NCLOB
    | NEEDED
    | NESTED
    | NESTED_TABLE_FAST_INSERT
    | NESTED_TABLE_GET_REFS
    | NESTED_TABLE_ID
    | NESTED_TABLE_SET_REFS
    | NESTED_TABLE_SET_SETID
    | NETWORK
    | NEVER
    | NEW
    | NEW_TIME
    | NEXT_DAY
    | NEXT
    | NL_AJ
    | NLJ_BATCHING
    | NLJ_INDEX_FILTER
    | NLJ_INDEX_SCAN
    | NLJ_PREFETCH
    | NLS_CALENDAR
    | NLS_CHARACTERSET
    | NLS_CHARSET_DECL_LEN
    | NLS_CHARSET_ID
    | NLS_CHARSET_NAME
    | NLS_COMP
    | NLS_CURRENCY
    | NLS_DATE_FORMAT
    | NLS_DATE_LANGUAGE
    | NLS_INITCAP
    | NLS_ISO_CURRENCY
    | NL_SJ
    | NLS_LANG
    | NLS_LANGUAGE
    | NLS_LENGTH_SEMANTICS
    | NLS_LOWER
    | NLS_NCHAR_CONV_EXCP
    | NLS_NUMERIC_CHARACTERS
    | NLS_SORT
    | NLSSORT
    | NLS_SPECIAL_CHARS
    | NLS_TERRITORY
    | NLS_UPPER
    | NO_ACCESS
    | NOAPPEND
    | NOARCHIVELOG
    | NOAUDIT
    | NO_BASETABLE_MULTIMV_REWRITE
    | NO_BIND_AWARE
    | NO_BUFFER
    | NOCACHE
    | NO_CARTESIAN
    | NO_CHECK_ACL_REWRITE
    | NO_CLUSTER_BY_ROWID
    | NO_COALESCE_SQ
    | NO_CONNECT_BY_CB_WHR_ONLY
    | NO_CONNECT_BY_COMBINE_SW
    | NO_CONNECT_BY_COST_BASED
    | NO_CONNECT_BY_ELIM_DUPS
    | NO_CONNECT_BY_FILTERING
    | NO_COST_XML_QUERY_REWRITE
    | NO_CPU_COSTING
    | NOCPU_COSTING
    | NOCYCLE
    | NODELAY
    | NO_DOMAIN_INDEX_FILTER
    | NO_DST_UPGRADE_INSERT_CONV
    | NO_ELIMINATE_JOIN
    | NO_ELIMINATE_OBY
    | NO_ELIMINATE_OUTER_JOIN
    | NOENTITYESCAPING
    | NO_EXPAND_GSET_TO_UNION
    | NO_EXPAND
    | NO_EXPAND_TABLE
    | NO_FACT
    | NO_FACTORIZE_JOIN
    | NO_FILTERING
    | NOFORCE
    | NO_FULL_OUTER_JOIN_TO_OUTER
    | NO_GBY_PUSHDOWN
    | NOGUARANTEE
    | NO_INDEX_FFS
    | NO_INDEX
    | NO_INDEX_SS
    | NO_LOAD
    | NOLOCAL
    | NOLOGGING
    | NOMAPPING
    | NOMAXVALUE
    | NO_MERGE
    | NOMINIMIZE
    | NOMINVALUE
    | NO_MODEL_PUSH_REF
    | NO_MONITORING
    | NOMONITORING
    | NO_MONITOR
    | NO_MULTIMV_REWRITE
    | NO
    | NO_NATIVE_FULL_OUTER_JOIN
    | NONBLOCKING
    | NONE
    | NO_NLJ_BATCHING
    | NO_NLJ_PREFETCH
    | NONSCHEMA
    | NOORDER
    | NO_ORDER_ROLLUPS
    | NO_OUTER_JOIN_TO_ANTI
    | NO_OUTER_JOIN_TO_INNER
    | NOOVERRIDE
    | NO_PARALLEL_INDEX
    | NOPARALLEL_INDEX
    | NO_PARALLEL
    | NOPARALLEL
    | NO_PARTIAL_COMMIT
    | NO_PLACE_DISTINCT
    | NO_PLACE_GROUP_BY
    | NO_PQ_MAP
    | NO_PRUNE_GSETS
    | NO_PULL_PRED
    | NO_PUSH_PRED
    | NO_PUSH_SUBQ
    | NO_PX_JOIN_FILTER
    | NO_QKN_BUFF
    | NO_QUERY_TRANSFORMATION
    | NO_REF_CASCADE
    | NORELY
    | NOREPAIR
    | NORESETLOGS
    | NO_RESULT_CACHE
    | NOREVERSE
    | NO_REWRITE
    | NOREWRITE
    | NORMAL
    | NOROWDEPENDENCIES
    | NOSCHEMACHECK
    | NOSEGMENT
    | NO_SEMIJOIN
    | NO_SEMI_TO_INNER
    | NO_SET_TO_JOIN
    | NOSORT
    | NO_SQL_TUNE
    | NO_STAR_TRANSFORMATION
    | NO_STATEMENT_QUEUING
    | NO_STATS_GSETS
    | NOSTRICT
    | NO_SUBQUERY_PRUNING
    | NO_SUBSTRB_PAD
    | NO_SWAP_JOIN_INPUTS
    | NOSWITCH
    | NO_TABLE_LOOKUP_BY_NL
    | NO_TEMP_TABLE
    | NOTHING
    | NOTIFICATION
    | NO_TRANSFORM_DISTINCT_AGG
    | NO_UNNEST
    | NO_USE_HASH_AGGREGATION
    | NO_USE_HASH_GBY_FOR_PUSHDOWN
    | NO_USE_HASH
    | NO_USE_INVISIBLE_INDEXES
    | NO_USE_MERGE
    | NO_USE_NL
    | NOVALIDATE
    | NO_XDB_FASTPATH_INSERT
    | NO_XML_DML_REWRITE
    | NO_XMLINDEX_REWRITE_IN_SELECT
    | NO_XMLINDEX_REWRITE
    | NO_XML_QUERY_REWRITE
    | NTH_VALUE
    | NTILE
    | NULLIF
    | NULLS
    | NUMERIC
    | NUM_INDEX_KEYS
    | NUMTODSINTERVAL
    | NUMTOYMINTERVAL
    | NVARCHAR2
    | NVL2
    | NVL
    | OBJECT2XML
    | OBJECT
    | OBJNO
    | OBJNO_REUSE
    | OCCURENCES
    | OFFLINE
    | OFF
    | OIDINDEX
    | OID
    | OLAP
    | OLD
    | OLD_PUSH_PRED
    | OLTP
    | ONLINE
    | ONLY
    | OPAQUE
    | OPAQUE_TRANSFORM
    | OPAQUE_XCANONICAL
    | OPCODE
    | OPEN
    | OPERATIONS
    | OPERATOR
    | OPT_ESTIMATE
    | OPTIMAL
    | OPTIMIZE
    | OPTIMIZER_FEATURES_ENABLE
    | OPTIMIZER_GOAL
    | OPT_PARAM
    | ORA_BRANCH
    | ORADEBUG
    | ORA_DST_AFFECTED
    | ORA_DST_CONVERT
    | ORA_DST_ERROR
    | ORA_GET_ACLIDS
    | ORA_GET_PRIVILEGES
    | ORA_HASH
    | ORA_ROWSCN
    | ORA_ROWSCN_RAW
    | ORA_ROWVERSION
    | ORA_TABVERSION
    | ORDERED
    | ORDERED_PREDICATES
    | ORDINALITY
    | OR_EXPAND
    | ORGANIZATION
    | OR_PREDICATES
    | OTHER
    | OUTER_JOIN_TO_ANTI
    | OUTER_JOIN_TO_INNER
    | OUTER
    | OUTLINE_LEAF
    | OUTLINE
    | OUT_OF_LINE
    | OVERFLOW_
    | OVERFLOW_NOMOVE
    | OVERLAPS
    | OVER
    | OWNER
    | OWNERSHIP
    | OWN
    | PACKAGE
    | PACKAGES
    | PARALLEL_INDEX
    | PARALLEL
    | PARAMETERS
    | PARAM
    | PARENT
    | PARITY
    | PARTIALLY
    | PARTITION_HASH
    | PARTITION_LIST
    | PARTITION
    | PARTITION_RANGE
    | PARTITIONS
    | PARTNUMINST
    | PASSING
    | PASSWORD_GRACE_TIME
    | PASSWORD_LIFE_TIME
    | PASSWORD_LOCK_TIME
    | PASSWORD
    | PASSWORD_REUSE_MAX
    | PASSWORD_REUSE_TIME
    | PASSWORD_VERIFY_FUNCTION
    | PATH
    | PATHS
    | PBL_HS_BEGIN
    | PBL_HS_END
    | PCTINCREASE
    | PCTTHRESHOLD
    | PCTUSED
    | PCTVERSION
    | PENDING
    | PERCENTILE_CONT
    | PERCENTILE_DISC
    | PERCENT_KEYWORD
    | PERCENT_RANKM
    | PERCENT_RANK
    | PERFORMANCE
    | PERMANENT
    | PERMISSION
    | PFILE
    | PHYSICAL
    | PIKEY
    | PIV_GB
    | PIVOT
    | PIV_SSF
    | PLACE_DISTINCT
    | PLACE_GROUP_BY
    | PLAN
    | PLSCOPE_SETTINGS
    | PLSQL_CCFLAGS
    | PLSQL_CODE_TYPE
    | PLSQL_DEBUG
    | PLSQL_OPTIMIZE_LEVEL
    | PLSQL_WARNINGS
    | POINT
    | POLICY
    | POST_TRANSACTION
    | POWERMULTISET_BY_CARDINALITY
    | POWERMULTISET
    | POWER
    | PQ_DISTRIBUTE
    | PQ_MAP
    | PQ_NOMAP
    | PREBUILT
    | PRECEDES
    | PRECEDING
    | PRECISION
    | PRECOMPUTE_SUBQUERY
    | PREDICATE_REORDERS
    | PREDICTION_BOUNDS
    | PREDICTION_COST
    | PREDICTION_DETAILS
    | PREDICTION
    | PREDICTION_PROBABILITY
    | PREDICTION_SET
    | PREPARE
    | PRESENT
    | PRESENTNNV
    | PRESENTV
    | PRESERVE
    | PRESERVE_OID
    | PREVIOUS
    | PRIMARY
    | PRIVATE
    | PRIVATE_SGA
    | PRIVILEGE
    | PRIVILEGES
    | PROCEDURAL
    | PROCEDURE
    | PROCESS
    | PROFILE
    | PROGRAM
    | PROJECT
    | PROPAGATE
    | PROTECTED
    | PROTECTION
    | PULL_PRED
    | PURGE
    | PUSH_PRED
    | PUSH_SUBQ
    | PX_GRANULE
    | PX_JOIN_FILTER
    | QB_NAME
    | QUERY_BLOCK
    | QUERY
    | QUEUE_CURR
    | QUEUE
    | QUEUE_ROWP
    | QUIESCE
    | QUORUM
    | QUOTA
    | QUARTER
    | RANDOM_LOCAL
    | RANDOM
    | RANGE
    | RANKM
    | RANK
    | RAPIDLY
    | RATIO_TO_REPORT
    | RAWTOHEX
    | RAWTONHEX
    | RBA
    | RBO_OUTLINE
    | RDBA
    | READ
    | READS
    | REAL
    | REBALANCE
    | REBUILD
    | RECORDS_PER_BLOCK
    | RECOVERABLE
    | RECOVER
    | RECOVERY
    | RECYCLEBIN
    | RECYCLE
    | REDACTION
    | REDO
    | REDUCED
    | REDUNDANCY
    | REF_CASCADE_CURSOR
    | REFERENCED
    | REFERENCE
    | REFERENCES
    | REFERENCING
    | REF
    | REFRESH
    | REFTOHEX
    | REGEXP_COUNT
    | REGEXP_INSTR
    | REGEXP_LIKE
    | REGEXP_REPLACE
    | REGEXP_SUBSTR
    | REGISTER
    | REGR_AVGX
    | REGR_AVGY
    | REGR_COUNT
    | REGR_INTERCEPT
    | REGR_R2
    | REGR_SLOPE
    | REGR_SXX
    | REGR_SXY
    | REGR_SYY
    | REGULAR
    | REJECT
    | REKEY
    | RELATIONAL
    | RELY
    | REMAINDER
    | REMOTE_MAPPED
    | REMOVE
    | REPAIR
    | REPEAT
    | REPLACE
    | REPLICATION
    | REQUIRED
    | RESETLOGS
    | RESET
    | RESIZE
    | RESOLVE
    | RESOLVER
    | RESPECT
    | RESTORE_AS_INTERVALS
    | RESTORE
    | RESTRICT_ALL_REF_CONS
    | RESTRICTED
    | RESTRICT
    | RESULT_CACHE
    | RESUMABLE
    | RESUME
    | RETENTION
    | RETRY_ON_ROW_CHANGE
    | RETURNING
    | RETURN
    | REUSE
    | REVERSE
    | REWRITE
    | REWRITE_OR_ERROR
    | RIGHT
    | ROLE
    | ROLES
    | ROLLBACK
    | ROLLING
    | ROLLUP
    | ROUND
    | ROWDEPENDENCIES
    | ROWID
    | ROWIDTOCHAR
    | ROWIDTONCHAR
    | ROW_LENGTH
    | ROW
    | ROW_NUMBER
    | ROWNUM
    | ROWS
    | RPAD
    | RTRIM
    | RULE
    | RULES
    | SALT
    | SAMPLE
    | SAVE_AS_INTERVALS
    | SAVEPOINT
    | SB4
    | SCALE
    | SCALE_ROWS
    | SCAN_INSTANCES
    | SCAN
    | SCHEDULER
    | SCHEMACHECK
    | SCHEMA
    | SCN_ASCENDING
    | SCN
    | SCOPE
    | SD_ALL
    | SD_INHIBIT
    | SD_SHOW
    | SEARCH
    | SECOND
    | SECUREFILE_DBA
    | SECUREFILE
    | SECURITY
    | SEED
    | SEG_BLOCK
    | SEG_FILE
    | SEGMENT
    | SELECTIVITY
    | SEMIJOIN_DRIVER
    | SEMIJOIN
    | SEMI_TO_INNER
    | SEQUENCED
    | SEQUENCE
    | SEQUENTIAL
    | SERIALIZABLE
    | SERVERERROR
    | SESSION_CACHED_CURSORS
    | SESSION
    | SESSIONS_PER_USER
    | SESSIONTIMEZONE
    | SESSIONTZNAME
    | SETS
    | SETTINGS
    | SET_TO_JOIN
    | SEVERE
    | SHARED
    | SHARED_POOL
    | SHOW
    | SHRINK
    | SHUTDOWN
    | SIBLINGS
    | SID
    | SIGNAL_COMPONENT
    | SIGNAL_FUNCTION
    | SIGN
    | SIMPLE
    | SINGLE
    | SINGLETASK
    | SINH
    | SIN
    | SKIP_EXT_OPTIMIZER
    | SKIP_
    | SKIP_UNQ_UNUSABLE_IDX
    | SKIP_UNUSABLE_INDEXES
    | SMALLFILE
    | SNAPSHOT
    | SOME
    | SORT
    | SOUNDEX
    | SOURCE
    | SPACE_KEYWORD
    | SPECIFICATION
    | SPFILE
    | SPLIT
    | SPREADSHEET
    | SQLLDR
    | SQL
    | SQL_TRACE
    | SQRT
    | STALE
    | STANDALONE
    | STANDBY_MAX_DATA_DELAY
    | STANDBY
    | STAR
    | STAR_TRANSFORMATION
    | STARTUP
    | STATEMENT_ID
    | STATEMENT_QUEUING
    | STATEMENTS
    | STATIC
    | STATISTICS
    | STATS_BINOMIAL_TEST
    | STATS_CROSSTAB
    | STATS_F_TEST
    | STATS_KS_TEST
    | STATS_MODE
    | STATS_MW_TEST
    | STATS_ONE_WAY_ANOVA
    | STATS_T_TEST_INDEP
    | STATS_T_TEST_INDEPU
    | STATS_T_TEST_ONE
    | STATS_T_TEST_PAIRED
    | STATS_WSR_TEST
    | STDDEV
    | STDDEV_POP
    | STDDEV_SAMP
    | STOP
    | STORAGE
    | STORE
    | STREAMS
    | STRICT
    | STRING
    | STRIPE_COLUMNS
    | STRIPE_WIDTH
    | STRIP
    | STRUCTURE
    | SUBMULTISET
    | SUBPARTITION
    | SUBPARTITION_REL
    | SUBPARTITIONS
    | SUBQUERIES
    | SUBQUERY_PRUNING
    | SUBSTITUTABLE
    | SUBSTR2
    | SUBSTR4
    | SUBSTRB
    | SUBSTRC
    | SUBSTR
    | SUCCESSFUL
    | SUMMARY
    | SUM
    | SUPPLEMENTAL
    | SUSPEND
    | SWAP_JOIN_INPUTS
    | SWITCH
    | SWITCHOVER
    | SYNCHRONOUS
    | SYNC
    | SYSASM
    | SYS_AUDIT
    | SYSAUX
    | SYS_CHECKACL
    | SYS_CONNECT_BY_PATH
    | SYS_CONTEXT
    | SYSDATE
    | SYSDBA
    | SYS_DBURIGEN
    | SYS_DL_CURSOR
    | SYS_DM_RXFORM_CHR
    | SYS_DM_RXFORM_NUM
    | SYS_DOM_COMPARE
    | SYS_DST_PRIM2SEC
    | SYS_DST_SEC2PRIM
    | SYS_ET_BFILE_TO_RAW
    | SYS_ET_BLOB_TO_IMAGE
    | SYS_ET_IMAGE_TO_BLOB
    | SYS_ET_RAW_TO_BFILE
    | SYS_EXTPDTXT
    | SYS_EXTRACT_UTC
    | SYS_FBT_INSDEL
    | SYS_FILTER_ACLS
    | SYS_FNMATCHES
    | SYS_FNREPLACE
    | SYS_GET_ACLIDS
    | SYS_GET_PRIVILEGES
    | SYS_GETTOKENID
    | SYS_GETXTIVAL
    | SYS_GUID
    | SYS_MAKEXML
    | SYS_MAKE_XMLNODEID
    | SYS_MKXMLATTR
    | SYS_OP_ADT2BIN
    | SYS_OP_ADTCONS
    | SYS_OP_ALSCRVAL
    | SYS_OP_ATG
    | SYS_OP_BIN2ADT
    | SYS_OP_BITVEC
    | SYS_OP_BL2R
    | SYS_OP_BLOOM_FILTER_LIST
    | SYS_OP_BLOOM_FILTER
    | SYS_OP_C2C
    | SYS_OP_CAST
    | SYS_OP_CEG
    | SYS_OP_CL2C
    | SYS_OP_COMBINED_HASH
    | SYS_OP_COMP
    | SYS_OP_CONVERT
    | SYS_OP_COUNTCHG
    | SYS_OP_CSCONV
    | SYS_OP_CSCONVTEST
    | SYS_OP_CSR
    | SYS_OP_CSX_PATCH
    | SYS_OP_DECOMP
    | SYS_OP_DESCEND
    | SYS_OP_DISTINCT
    | SYS_OP_DRA
    | SYS_OP_DUMP
    | SYS_OP_DV_CHECK
    | SYS_OP_ENFORCE_NOT_NULL
    | SYSOPER
    | SYS_OP_EXTRACT
    | SYS_OP_GROUPING
    | SYS_OP_GUID
    | SYS_OP_IIX
    | SYS_OP_ITR
    | SYS_OP_LBID
    | SYS_OP_LOBLOC2BLOB
    | SYS_OP_LOBLOC2CLOB
    | SYS_OP_LOBLOC2ID
    | SYS_OP_LOBLOC2NCLOB
    | SYS_OP_LOBLOC2TYP
    | SYS_OP_LSVI
    | SYS_OP_LVL
    | SYS_OP_MAKEOID
    | SYS_OP_MAP_NONNULL
    | SYS_OP_MSR
    | SYS_OP_NICOMBINE
    | SYS_OP_NIEXTRACT
    | SYS_OP_NII
    | SYS_OP_NIX
    | SYS_OP_NOEXPAND
    | SYS_OP_NTCIMG
    | SYS_OP_NUMTORAW
    | SYS_OP_OIDVALUE
    | SYS_OP_OPNSIZE
    | SYS_OP_PAR_1
    | SYS_OP_PARGID_1
    | SYS_OP_PARGID
    | SYS_OP_PAR
    | SYS_OP_PIVOT
    | SYS_OP_R2O
    | SYS_OP_RAWTONUM
    | SYS_OP_RDTM
    | SYS_OP_REF
    | SYS_OP_RMTD
    | SYS_OP_ROWIDTOOBJ
    | SYS_OP_RPB
    | SYS_OPTLOBPRBSC
    | SYS_OP_TOSETID
    | SYS_OP_TPR
    | SYS_OP_TRTB
    | SYS_OPTXICMP
    | SYS_OPTXQCASTASNQ
    | SYS_OP_UNDESCEND
    | SYS_OP_VECAND
    | SYS_OP_VECBIT
    | SYS_OP_VECOR
    | SYS_OP_VECXOR
    | SYS_OP_VERSION
    | SYS_OP_VREF
    | SYS_OP_VVD
    | SYS_OP_XMLCONS_FOR_CSX
    | SYS_OP_XPTHATG
    | SYS_OP_XPTHIDX
    | SYS_OP_XPTHOP
    | SYS_OP_XTXT2SQLT
    | SYS_ORDERKEY_DEPTH
    | SYS_ORDERKEY_MAXCHILD
    | SYS_ORDERKEY_PARENT
    | SYS_PARALLEL_TXN
    | SYS_PATHID_IS_ATTR
    | SYS_PATHID_IS_NMSPC
    | SYS_PATHID_LASTNAME
    | SYS_PATHID_LASTNMSPC
    | SYS_PATH_REVERSE
    | SYS_PXQEXTRACT
    | SYS_RID_ORDER
    | SYS_ROW_DELTA
    | SYS_SC_2_XMLT
    | SYS_SYNRCIREDO
    | SYSTEM_DEFINED
    | SYSTEM
    | SYSTIMESTAMP
    | SYS_TYPEID
    | SYS_UMAKEXML
    | SYS_XMLANALYZE
    | SYS_XMLCONTAINS
    | SYS_XMLCONV
    | SYS_XMLEXNSURI
    | SYS_XMLGEN
    | SYS_XMLI_LOC_ISNODE
    | SYS_XMLI_LOC_ISTEXT
    | SYS_XMLINSTR
    | SYS_XMLLOCATOR_GETSVAL
    | SYS_XMLNODEID_GETCID
    | SYS_XMLNODEID_GETLOCATOR
    | SYS_XMLNODEID_GETOKEY
    | SYS_XMLNODEID_GETPATHID
    | SYS_XMLNODEID_GETPTRID
    | SYS_XMLNODEID_GETRID
    | SYS_XMLNODEID_GETSVAL
    | SYS_XMLNODEID_GETTID
    | SYS_XMLNODEID
    | SYS_XMLT_2_SC
    | SYS_XMLTRANSLATE
    | SYS_XMLTYPE2SQL
    | SYS_XQ_ASQLCNV
    | SYS_XQ_ATOMCNVCHK
    | SYS_XQBASEURI
    | SYS_XQCASTABLEERRH
    | SYS_XQCODEP2STR
    | SYS_XQCODEPEQ
    | SYS_XQCON2SEQ
    | SYS_XQCONCAT
    | SYS_XQDELETE
    | SYS_XQDFLTCOLATION
    | SYS_XQDOC
    | SYS_XQDOCURI
    | SYS_XQDURDIV
    | SYS_XQED4URI
    | SYS_XQENDSWITH
    | SYS_XQERRH
    | SYS_XQERR
    | SYS_XQESHTMLURI
    | SYS_XQEXLOBVAL
    | SYS_XQEXSTWRP
    | SYS_XQEXTRACT
    | SYS_XQEXTRREF
    | SYS_XQEXVAL
    | SYS_XQFB2STR
    | SYS_XQFNBOOL
    | SYS_XQFNCMP
    | SYS_XQFNDATIM
    | SYS_XQFNLNAME
    | SYS_XQFNNM
    | SYS_XQFNNSURI
    | SYS_XQFNPREDTRUTH
    | SYS_XQFNQNM
    | SYS_XQFNROOT
    | SYS_XQFORMATNUM
    | SYS_XQFTCONTAIN
    | SYS_XQFUNCR
    | SYS_XQGETCONTENT
    | SYS_XQINDXOF
    | SYS_XQINSERT
    | SYS_XQINSPFX
    | SYS_XQIRI2URI
    | SYS_XQLANG
    | SYS_XQLLNMFRMQNM
    | SYS_XQMKNODEREF
    | SYS_XQNILLED
    | SYS_XQNODENAME
    | SYS_XQNORMSPACE
    | SYS_XQNORMUCODE
    | SYS_XQ_NRNG
    | SYS_XQNSP4PFX
    | SYS_XQNSPFRMQNM
    | SYS_XQPFXFRMQNM
    | SYS_XQ_PKSQL2XML
    | SYS_XQPOLYABS
    | SYS_XQPOLYADD
    | SYS_XQPOLYCEL
    | SYS_XQPOLYCSTBL
    | SYS_XQPOLYCST
    | SYS_XQPOLYDIV
    | SYS_XQPOLYFLR
    | SYS_XQPOLYMOD
    | SYS_XQPOLYMUL
    | SYS_XQPOLYRND
    | SYS_XQPOLYSQRT
    | SYS_XQPOLYSUB
    | SYS_XQPOLYUMUS
    | SYS_XQPOLYUPLS
    | SYS_XQPOLYVEQ
    | SYS_XQPOLYVGE
    | SYS_XQPOLYVGT
    | SYS_XQPOLYVLE
    | SYS_XQPOLYVLT
    | SYS_XQPOLYVNE
    | SYS_XQREF2VAL
    | SYS_XQRENAME
    | SYS_XQREPLACE
    | SYS_XQRESVURI
    | SYS_XQRNDHALF2EVN
    | SYS_XQRSLVQNM
    | SYS_XQRYENVPGET
    | SYS_XQRYVARGET
    | SYS_XQRYWRP
    | SYS_XQSEQ2CON4XC
    | SYS_XQSEQ2CON
    | SYS_XQSEQDEEPEQ
    | SYS_XQSEQINSB
    | SYS_XQSEQRM
    | SYS_XQSEQRVS
    | SYS_XQSEQSUB
    | SYS_XQSEQTYPMATCH
    | SYS_XQSTARTSWITH
    | SYS_XQSTATBURI
    | SYS_XQSTR2CODEP
    | SYS_XQSTRJOIN
    | SYS_XQSUBSTRAFT
    | SYS_XQSUBSTRBEF
    | SYS_XQTOKENIZE
    | SYS_XQTREATAS
    | SYS_XQ_UPKXML2SQL
    | SYS_XQXFORM
    | TABLE
    | TABLE_LOOKUP_BY_NL
    | TABLES
    | TABLESPACE
    | TABLESPACE_NO
    | TABLE_STATS
    | TABNO
    | TANH
    | TAN
    | TBLORIDXPARTNUM
    | TEMPFILE
    | TEMPLATE
    | TEMPORARY
    | TEMP_TABLE
    | TEST_
    | THAN
    | THE
    | THEN
    | THREAD
    | THROUGH
    | TIME
    | TIMEOUT
    | TIMES
    | TIMESTAMP
    | TIMEZONE_ABBR
    | TIMEZONE_HOUR
    | TIMEZONE_MINUTE
    | TIME_ZONE
    | TIMEZONE_OFFSET
    | TIMEZONE_REGION
    | TIV_GB
    | TIV_SSF
    | TO_BINARY_DOUBLE
    | TO_BINARY_FLOAT
    | TO_BLOB
    | TO_CHAR
    | TO_CLOB
    | TO_DATE
    | TO_DSINTERVAL
    | TO_LOB
    | TO_MULTI_BYTE
    | TO_NCHAR
    | TO_NCLOB
    | TO_NUMBER
    | TOPLEVEL
    | TO_SINGLE_BYTE
    | TO_TIME
    | TO_TIMESTAMP
    | TO_TIMESTAMP_TZ
    | TO_TIME_TZ
    | TO_YMINTERVAL
    | TRACE
    | TRACING
    | TRACKING
    | TRAILING
    | TRANSACTION
    | TRANSFORM_DISTINCT_AGG
    | TRANSITIONAL
    | TRANSITION
    | TRANSLATE
    | TREAT
    | TRIGGERS
    | TRIM
    | TRUE
    | TRUNCATE
    | TRUNC
    | TRUSTED
    | TUNING
    | TX
    | TYPE
    | TYPES
    | TZ_OFFSET
    | UB2
    | UBA
    | UID
    | UNARCHIVED
    | UNBOUNDED
    | UNBOUND
    | UNDER
    | UNDO
    | UNDROP
    | UNIFORM
    | UNISTR
    | UNLIMITED
    | UNLOAD
    | UNLOCK
    | UNNEST_INNERJ_DISTINCT_VIEW
    | UNNEST
    | UNNEST_NOSEMIJ_NODISTINCTVIEW
    | UNNEST_SEMIJ_VIEW
    | UNPACKED
    | UNPIVOT
    | UNPROTECTED
    | UNQUIESCE
    | UNRECOVERABLE
    | UNRESTRICTED
    | UNTIL
    | UNUSABLE
    | UNUSED
    | UPDATABLE
    | UPDATED
    | UPDATEXML
    | UPD_INDEXES
    | UPD_JOININDEX
    | UPGRADE
    | UPPER
    | UPSERT
    | UROWID
    | USAGE
    | USE_ANTI
    | USE_CONCAT
    | USE_HASH_AGGREGATION
    | USE_HASH_GBY_FOR_PUSHDOWN
    | USE_HASH
    | USE_INVISIBLE_INDEXES
    | USE_MERGE_CARTESIAN
    | USE_MERGE
    | USE
    | USE_NL
    | USE_NL_WITH_INDEX
    | USE_PRIVATE_OUTLINES
    | USER_DEFINED
    | USERENV
    | USERGROUP
    | USER
    | USER_RECYCLEBIN
    | USERS
    | USE_SEMI
    | USE_STORED_OUTLINES
    | USE_TTT_FOR_GSETS
    | USE_WEAK_NAME_RESL
    | USING
    | VALIDATE
    | VALIDATION
    | VALUE
    | VARIANCE
    | VAR_POP
    | VARRAY
    | VARRAYS
    | VAR_SAMP
    | VARYING
    | VECTOR_READ
    | VECTOR_READ_TRACE
    | VERIFY
    | VERSIONING
    | VERSION
    | VERSIONS_ENDSCN
    | VERSIONS_ENDTIME
    | VERSIONS
    | VERSIONS_OPERATION
    | VERSIONS_STARTSCN
    | VERSIONS_STARTTIME
    | VERSIONS_XID
    | VIRTUAL
    | VISIBLE
    | VOLUME
    | VSIZE
    | WAIT
    | WALLET
    | WELLFORMED
    | WHENEVER
    | WHEN
    | WHITESPACE
    | WIDTH_BUCKET
    | WITHIN
    | WITHOUT
    | WORK
    | WRAPPED
    | WRITE
    | XDB_FASTPATH_INSERT
    | X_DYN_PRUNE
    | XID
    | XML2OBJECT
    | XMLATTRIBUTES
    | XMLCAST
    | XMLCDATA
    | XMLCOLATTVAL
    | XMLCOMMENT
    | XMLCONCAT
    | XMLDIFF
    | XML_DML_RWT_STMT
    | XMLELEMENT
    | XMLEXISTS2
    | XMLEXISTS
    | XMLFOREST
    | XMLINDEX_REWRITE_IN_SELECT
    | XMLINDEX_REWRITE
    | XMLINDEX_SEL_IDX_TBL
    | XMLISNODE
    | XMLISVALID
    | XML
    | XMLNAMESPACES
    | XMLPARSE
    | XMLPATCH
    | XMLPI
    | XMLQUERY
    | XMLQUERYVAL
    | XMLROOT
    | XMLSCHEMA
    | XMLSERIALIZE
    | XMLTABLE
    | XMLTRANSFORMBLOB
    | XMLTRANSFORM
    | XMLTYPE
    | XPATHTABLE
    | XS_SYS_CONTEXT
    | YEAR
    | YES
    | ZONE
    ;

special_id: LEFT_CURLY_BRACKET (numeric | regular_id) RIGHT_CURLY_BRACKET; // {1},{2},{4} .etc

special_property: LEFT_BRACKET (id_expression | special_id) RIGHT_BRACKET;

extract_function
    : (YEAR | QUARTER | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND | MILLISECOND) LEFT_PAREN concatenation RIGHT_PAREN
    ;

dates_function
    : date_diff_function
    | date_add_function
    | date_sub_function
    | date_trunc_function
    | date_function
    ;

date_diff_function
    : (YEAR_DIFF | MONTH_DIFF | WEEK_DIFF | DAY_DIFF | HOUR_DIFF | MINUTE_DIFF | SECOND_DIFF)
      LEFT_PAREN concatenation COMMA concatenation RIGHT_PAREN;

date_add_function
    : (YEAR_ADD | MONTH_ADD | WEEK_ADD | DAY_ADD | HOUR_ADD | MINUTE_ADD | SECOND_ADD)
      LEFT_PAREN concatenation COMMA UNSIGNED_INTEGER RIGHT_PAREN;
date_sub_function
    : (YEAR_SUB | MONTH_SUB | WEEK_SUB | DAY_SUB | HOUR_SUB | MINUTE_SUB | SECOND_SUB)
      LEFT_PAREN concatenation COMMA UNSIGNED_INTEGER RIGHT_PAREN;

percentile_function
    : (PERCENT_KEYWORD | PERCENTILE | PERCENTILE_CONT | PERCENTILE_DISC)
      LEFT_PAREN concatenation COMMA concatenation (COMMA (ASC | DESC))? RIGHT_PAREN
    | (PERCENTILE_CONT | PERCENTILE_DISC)
      LEFT_PAREN concatenation RIGHT_PAREN WITHIN GROUP LEFT_PAREN order_by_clause RIGHT_PAREN;

median_function
    : MEDIAN LEFT_PAREN concatenation RIGHT_PAREN
    ;

log_function
    : LOG LEFT_PAREN concatenation (COMMA concatenation)? RIGHT_PAREN
    | LOG10 LEFT_PAREN concatenation RIGHT_PAREN
    ;

rank_function
    : (RANK | DENSE_RANK) LEFT_PAREN concatenation (COMMA (ASC | DESC))? RIGHT_PAREN
    | (RANK | DENSE_RANK) LEFT_PAREN RIGHT_PAREN OVER LEFT_PAREN ORDER BY concatenation (ASC | DESC)? RIGHT_PAREN
    ;

stddev_function:   STDDEV LEFT_PAREN (ALL | DISTINCT)? concatenation RIGHT_PAREN;
variance_function: VARIANCE LEFT_PAREN (ALL | DISTINCT)? concatenation RIGHT_PAREN;
coalesce_function: COALESCE LEFT_PAREN concatenation (COMMA concatenation)+ RIGHT_PAREN;
decode_function:   DECODE LEFT_PAREN concatenation COMMA concatenation (COMMA concatenation)+  RIGHT_PAREN;

floor_function: FLOOR LEFT_PAREN concatenation RIGHT_PAREN;
ceil_function:  CEIL LEFT_PAREN concatenation RIGHT_PAREN;

trim_function
    : TRIM LEFT_PAREN (LEADING | TRAILING | BOTH)? quoted_string?  FROM concatenation RIGHT_PAREN
    | (TRIM | LTRIM | RTRIM) LEFT_PAREN concatenation (COMMA quoted_string)? RIGHT_PAREN
    ;

format_function
    : FORMAT LEFT_PAREN concatenation COMMA quoted_string RIGHT_PAREN
    | FORMAT LEFT_PAREN quoted_string (COMMA concatenation)+ RIGHT_PAREN
    ;

confidence_interval_function
    : CONFIDENCE_INTERVAL LEFT_PAREN concatenation COMMA concatenation (COMMA (UP | DOWN))? RIGHT_PAREN
    ;

raw_sql: RAW_SQL LEFT_PAREN quoted_string RIGHT_PAREN;

date_trunc_function
    : DATE_TRUNC LEFT_PAREN concatenation COMMA (YEAR | QUARTER | MONTH | WEEK | DAY | HOUR | MINUTE) RIGHT_PAREN
    ;

date_function: DATE LEFT_PAREN concatenation RIGHT_PAREN;

// LEXER

fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];

fragment CHINESE_CHARACTER : '\u4E00' .. '\u9FA5' | '\uF900' .. '\uFA2D'; // 中文字符

YEAR_DIFF:                    Y E A R '_' D I F F;
MONTH_DIFF:                   M O N T H '_' D I F F;
WEEK_DIFF:                    W E E K '_' D I F F;
DAY_DIFF:                     D A Y '_' D I F F;
HOUR_DIFF:                    H O U R '_' D I F F;
MINUTE_DIFF:                  M I N U T E '_' D I F F;
SECOND_DIFF:                  S E C O N D '_' D I F F;

YEAR_ADD:                     Y E A R '_' A D D;
MONTH_ADD:                    M O N T H '_' A D D;
WEEK_ADD:                     W E E K '_' A D D;
DAY_ADD:                      D A Y '_' A D D;
HOUR_ADD:                     H O U R '_' A D D;
MINUTE_ADD:                   M I N U T E '_' A D D;
SECOND_ADD:                   S E C O N D '_' A D D;

YEAR_SUB:                     Y E A R '_' S U B;
MONTH_SUB:                    M O N T H '_' S U B;
WEEK_SUB:                     W E E K '_' S U B;
DAY_SUB:                      D A Y '_' S U B;
HOUR_SUB:                     H O U R '_' S U B;
MINUTE_SUB:                   M I N U T E '_' S U B;
SECOND_SUB:                   S E C O N D '_' S U B;

ABORT:                        'ABORT';
ABS:                          A B S;
ACCESS:                       'ACCESS';
ACCESSED:                     'ACCESSED';
ACCOUNT:                      'ACCOUNT';
ACL:                          'ACL';
ACOS:                         A C O S;
ACTION:                       'ACTION';
ACTIONS:                      'ACTIONS';
ACTIVATE:                     'ACTIVATE';
ACTIVE:                       'ACTIVE';
ACTIVE_COMPONENT:             'ACTIVE_COMPONENT';
ACTIVE_DATA:                  'ACTIVE_DATA';
ACTIVE_FUNCTION:              'ACTIVE_FUNCTION';
ACTIVE_TAG:                   'ACTIVE_TAG';
ACTIVITY:                     'ACTIVITY';
ADAPTIVE_PLAN:                'ADAPTIVE_PLAN';
ADD:                          A D D;
ADD_COLUMN:                   'ADD_COLUMN';
ADD_GROUP:                    'ADD_GROUP';
ADJ_DATE:                     'ADJ_DATE';
ADMIN:                        'ADMIN';
ADMINISTER:                   'ADMINISTER';
ADMINISTRATOR:                'ADMINISTRATOR';
ADVANCED:                     'ADVANCED';
ADVISE:                       'ADVISE';
ADVISOR:                      'ADVISOR';
AFD_DISKSTRING:               'AFD_DISKSTRING';
AFTER:                        'AFTER';
AGENT:                        'AGENT';
AGGREGATE:                    'AGGREGATE';
A_LETTER:                     'A';
ALIAS:                        'ALIAS';
ALL:                          A L L;
ALLOCATE:                     'ALLOCATE';
ALLOW:                        'ALLOW';
ALL_ROWS:                     'ALL_ROWS';
ALTER:                        A L T E R;
ALWAYS:                       'ALWAYS';
ANALYZE:                      'ANALYZE';
ANCILLARY:                    'ANCILLARY';
AND:                          A N D;
AND_EQUAL:                    'AND_EQUAL';
ANOMALY:                      'ANOMALY';
ANSI_REARCH:                  'ANSI_REARCH';
ANTIJOIN:                     'ANTIJOIN';
ANY:                          A N Y;
ANYSCHEMA:                    'ANYSCHEMA';
APPEND:                       'APPEND';
APPENDCHILDXML:               'APPENDCHILDXML';
APPEND_VALUES:                'APPEND_VALUES';
APPLICATION:                  'APPLICATION';
APPLY:                        'APPLY';
APPROX_COUNT_DISTINCT:        'APPROX_COUNT_DISTINCT';
ARCHIVAL:                     'ARCHIVAL';
ARCHIVE:                      'ARCHIVE';
ARCHIVED:                     'ARCHIVED';
ARCHIVELOG:                   'ARCHIVELOG';
ARRAY:                        'ARRAY';
AS:                           A S;
ASC:                          A S C;
ASCII:                        'ASCII';
ASCIISTR:                     'ASCIISTR';
ASIN:                         A S I N;
ASIS:                         'ASIS';
ASSEMBLY:                     'ASSEMBLY';
ASSIGN:                       'ASSIGN';
ASSOCIATE:                    'ASSOCIATE';
ASYNC:                        'ASYNC';
ASYNCHRONOUS:                 'ASYNCHRONOUS';
ATAN2:                        A T A N '2';
ATAN:                         A T A N;
AT:                           'AT';
ATTRIBUTE:                    'ATTRIBUTE';
ATTRIBUTES:                   'ATTRIBUTES';
AUDIT:                        'AUDIT';
AUTHENTICATED:                'AUTHENTICATED';
AUTHENTICATION:               'AUTHENTICATION';
AUTHID:                       'AUTHID';
AUTHORIZATION:                'AUTHORIZATION';
AUTOALLOCATE:                 'AUTOALLOCATE';
AUTO:                         'AUTO';
AUTOEXTEND:                   'AUTOEXTEND';
AUTO_LOGIN:                   'AUTO_LOGIN';
AUTOMATIC:                    'AUTOMATIC';
AUTONOMOUS_TRANSACTION:       'AUTONOMOUS_TRANSACTION';
AUTO_REOPTIMIZE:              'AUTO_REOPTIMIZE';
AVAILABILITY:                 'AVAILABILITY';
AVRO:                         'AVRO';
BACKGROUND:                   'BACKGROUND';
BACKUP:                       'BACKUP';
BASIC:                        'BASIC';
BASICFILE:                    'BASICFILE';
BATCH:                        'BATCH';
BATCHSIZE:                    'BATCHSIZE';
BATCH_TABLE_ACCESS_BY_ROWID:  'BATCH_TABLE_ACCESS_BY_ROWID';
BECOME:                       'BECOME';
BEFORE:                       'BEFORE';
BEGIN:                        B E G I N;
BEGINNING:                    'BEGINNING';
BEGIN_OUTLINE_DATA:           'BEGIN_OUTLINE_DATA';
BEHALF:                       'BEHALF';
BEQUEATH:                     'BEQUEATH';
BETWEEN:                      B E T W E E N;
BFILE:                        B F I L E;
BFILENAME:                    'BFILENAME';
BIGFILE:                      'BIGFILE';
BIGINT:                       B I G I N T;
BINARY:                       B I N A R Y;
BINARY_DOUBLE:                B I N A R Y '_' D O U B L E;
BINARY_DOUBLE_INFINITY:       'BINARY_DOUBLE_INFINITY';
BINARY_DOUBLE_NAN:            'BINARY_DOUBLE_NAN';
BINARY_FLOAT:                 B I N A R Y '_' F L O A T;
BINARY_FLOAT_INFINITY:        'BINARY_FLOAT_INFINITY';
BINARY_FLOAT_NAN:             'BINARY_FLOAT_NAN';
BINARY_INTEGER:               'BINARY_INTEGER';
BIND_AWARE:                   'BIND_AWARE';
BINDING:                      'BINDING';
BIN_TO_NUM:                   'BIN_TO_NUM';
BITAND:                       B I T A N D;
BITMAP_AND:                   'BITMAP_AND';
BITMAP:                       'BITMAP';
BITMAPS:                      'BITMAPS';
BITMAP_TREE:                  'BITMAP_TREE';
BITS:                         'BITS';
BLOB:                         B L O B;
BLOCK:                        B L O C K;
BLOCK_RANGE:                  'BLOCK_RANGE';
BLOCKS:                       'BLOCKS';
BLOCKSIZE:                    'BLOCKSIZE';
BODY:                         'BODY';
BOOLEAN:                      B O O L E A N;
BOTH:                         B O T H;
BOUND:                        'BOUND';
BRANCH:                       'BRANCH';
BREADTH:                      'BREADTH';
BROADCAST:                    'BROADCAST';
BSON:                         B S O N;
BUFFER:                       'BUFFER';
BUFFER_CACHE:                 'BUFFER_CACHE';
BUFFER_POOL:                  'BUFFER_POOL';
BUILD:                        'BUILD';
BULK:                         'BULK';
BY:                           B Y;
BYPASS_RECURSIVE_CHECK:       'BYPASS_RECURSIVE_CHECK';
BYPASS_UJVC:                  'BYPASS_UJVC';
BYTE:                         'BYTE';
CACHE:                        'CACHE';
CACHE_CB:                     'CACHE_CB';
CACHE_INSTANCES:              'CACHE_INSTANCES';
CACHE_TEMP_TABLE:             'CACHE_TEMP_TABLE';
CACHING:                      'CACHING';
CALCULATED:                   'CALCULATED';
CALLBACK:                     'CALLBACK';
CALL:                         'CALL';
CANCEL:                       'CANCEL';
CANONICAL:                    'CANONICAL';
CAPACITY:                     'CAPACITY';
CARDINALITY:                  'CARDINALITY';
CASCADE:                      C A S C A D E;
CASE:                         C A S E;
CAST:                         C A S T;
CATEGORY:                     'CATEGORY';
CDBDEFAULT:                   'CDB$DEFAULT';
CEIL:                         C E I L (I N G) ?;
CELL_FLASH_CACHE:             'CELL_FLASH_CACHE';
CERTIFICATE:                  'CERTIFICATE';
CFILE:                        'CFILE';
CHAINED:                      'CHAINED';
CHANGE:                       'CHANGE';
CHANGE_DUPKEY_ERROR_INDEX:    'CHANGE_DUPKEY_ERROR_INDEX';
CHARACTER:                    C H A R A C T E R;
CHAR:                         C H A R;
CHAR_CS:                      'CHAR_CS';
CHARTOROWID:                  'CHARTOROWID';
CHECK_ACL_REWRITE:            'CHECK_ACL_REWRITE';
CHECK:                        'CHECK';
CHECKPOINT:                   'CHECKPOINT';
CHILD:                        'CHILD';
CHOOSE:                       'CHOOSE';
CHR:                          'CHR';
CHUNK:                        'CHUNK';
CLASS:                        'CLASS';
CLASSIFIER:                   'CLASSIFIER';
CLEANUP:                      'CLEANUP';
CLEAR:                        'CLEAR';
C_LETTER:                     'C';
CLIENT:                       'CLIENT';
CLOB:                         C L O B;
CLONE:                        'CLONE';
CLOSE_CACHED_OPEN_CURSORS:    'CLOSE_CACHED_OPEN_CURSORS';
CLOSE:                        'CLOSE';
CLUSTER_BY_ROWID:             'CLUSTER_BY_ROWID';
CLUSTER:                      C L U S T E R;
CLUSTER_DETAILS:              'CLUSTER_DETAILS';
CLUSTER_DISTANCE:             'CLUSTER_DISTANCE';
CLUSTER_ID:                   'CLUSTER_ID';
CLUSTERING:                   'CLUSTERING';
CLUSTERING_FACTOR:            'CLUSTERING_FACTOR';
CLUSTER_PROBABILITY:          'CLUSTER_PROBABILITY';
CLUSTER_SET:                  'CLUSTER_SET';
COALESCE:                     C O A L E S C E;
COALESCE_SQ:                  'COALESCE_SQ';
COARSE:                       'COARSE';
CO_AUTH_IND:                  'CO_AUTH_IND';
CONFIDENCE_INTERVAL:           C O N F I D E N C E '_' I N T E R V A L;
COLD:                         'COLD';
COLLECT:                      'COLLECT';
COLUMNAR:                     'COLUMNAR';
COLUMN_AUTH_INDICATOR:        'COLUMN_AUTH_INDICATOR';
COLUMN:                       C O L U M N;
COLUMNS:                      'COLUMNS';
COLUMN_STATS:                 'COLUMN_STATS';
COLUMN_VALUE:                 'COLUMN_VALUE';
COMMENT:                      C O M M E N T;
COMMIT:                       C O M M I T;
COMMITTED:                    'COMMITTED';
COMMON_DATA:                  'COMMON_DATA';
COMPACT:                      'COMPACT';
COMPATIBILITY:                'COMPATIBILITY';
COMPILE:                      'COMPILE';
COMPLETE:                     'COMPLETE';
COMPLIANCE:                   'COMPLIANCE';
COMPONENT:                    'COMPONENT';
COMPONENTS:                   'COMPONENTS';
COMPOSE:                      'COMPOSE';
COMPOSITE:                    'COMPOSITE';
COMPOSITE_LIMIT:              'COMPOSITE_LIMIT';
COMPOUND:                     'COMPOUND';
COMPRESS:                     'COMPRESS';
COMPUTE:                      'COMPUTE';
CONCAT:                       C O N C A T;
CON_DBID_TO_ID:               'CON_DBID_TO_ID';
CONDITIONAL:                  'CONDITIONAL';
CONDITION:                    'CONDITION';
CONFIRM:                      'CONFIRM';
CONFORMING:                   'CONFORMING';
CON_GUID_TO_ID:               'CON_GUID_TO_ID';
CON_ID:                       'CON_ID';
CON_NAME_TO_ID:               'CON_NAME_TO_ID';
CONNECT_BY_CB_WHR_ONLY:       'CONNECT_BY_CB_WHR_ONLY';
CONNECT_BY_COMBINE_SW:        'CONNECT_BY_COMBINE_SW';
CONNECT_BY_COST_BASED:        'CONNECT_BY_COST_BASED';
CONNECT_BY_ELIM_DUPS:         'CONNECT_BY_ELIM_DUPS';
CONNECT_BY_FILTERING:         'CONNECT_BY_FILTERING';
CONNECT_BY_ISCYCLE:           'CONNECT_BY_ISCYCLE';
CONNECT_BY_ISLEAF:            'CONNECT_BY_ISLEAF';
CONNECT_BY_ROOT:              'CONNECT_BY_ROOT';
CONNECT:                      C O N N E C T;
CONNECT_TIME:                 'CONNECT_TIME';
CONSIDER:                     'CONSIDER';
CONSISTENT:                   'CONSISTENT';
CONSTANT:                     'CONSTANT';
CONST:                        'CONST';
CONSTRAINT:                   C O N S T R A I N T;
CONSTRAINTS:                  C O N S T R A I N T S;
CONSTRUCTOR:                  'CONSTRUCTOR';
CONTAINER:                    'CONTAINER';
CONTAINER_DATA:               'CONTAINER_DATA';
CONTAINERS:                   'CONTAINERS';
CONTENT:                      C O N T E N T;
CONTENTS:                     'CONTENTS';
CONTEXT:                      'CONTEXT';
CONTINUE:                     'CONTINUE';
CONTROLFILE:                  'CONTROLFILE';
CON_UID_TO_ID:                'CON_UID_TO_ID';
CONVERT:                      'CONVERT';
COOKIE:                       'COOKIE';
COPY:                         'COPY';
CORR_K:                       'CORR_K';
CORR_S:                       'CORR_S';
CORRUPTION:                   'CORRUPTION';
CORRUPT_XID_ALL:              'CORRUPT_XID_ALL';
CORRUPT_XID:                  'CORRUPT_XID';
COS:                          C O S;
COSH:                         'COSH';
COST:                         'COST';
COST_XML_QUERY_REWRITE:       'COST_XML_QUERY_REWRITE';
COUNT:                        C O U N T;
COVAR_POP:                    'COVAR_POP';
COVAR_SAMP:                   'COVAR_SAMP';
CPU_COSTING:                  'CPU_COSTING';
CPU_PER_CALL:                 'CPU_PER_CALL';
CPU_PER_SESSION:              'CPU_PER_SESSION';
CRASH:                        'CRASH';
CREATE:                       C R E A T E;
CREATE_FILE_DEST:             'CREATE_FILE_DEST';
CREATE_STORED_OUTLINES:       'CREATE_STORED_OUTLINES';
CREATION:                     'CREATION';
CREDENTIAL:                   'CREDENTIAL';
CRITICAL:                     'CRITICAL';
CROSS:                        'CROSS';
CROSSEDITION:                 'CROSSEDITION';
CSCONVERT:                    'CSCONVERT';
CUBE_AJ:                      'CUBE_AJ';
CUBE:                         'CUBE';
CUBE_GB:                      'CUBE_GB';
CUBE_SJ:                      'CUBE_SJ';
CUME_DISTM:                   'CUME_DISTM';
CURRENT:                      'CURRENT';
CURRENT_DATE:                 'CURRENT_DATE';
CURRENT_SCHEMA:               'CURRENT_SCHEMA';
CURRENT_TIME:                 'CURRENT_TIME';
CURRENT_TIMESTAMP:            'CURRENT_TIMESTAMP';
CURRENT_USER:                 'CURRENT_USER';
CURRENTV:                     'CURRENTV';
CURSOR:                       'CURSOR';
CURSOR_SHARING_EXACT:         'CURSOR_SHARING_EXACT';
CURSOR_SPECIFIC_SEGMENT:      'CURSOR_SPECIFIC_SEGMENT';
CUSTOMDATUM:                  'CUSTOMDATUM';
CV:                           'CV';
CYCLE:                        C Y C L E;
DANGLING:                     'DANGLING';
DATABASE:                     'DATABASE';
DATA:                         'DATA';
DATAFILE:                     'DATAFILE';
DATAFILES:                    'DATAFILES';
DATAMOVEMENT:                 'DATAMOVEMENT';
DATAOBJNO:                    'DATAOBJNO';
DATAOBJ_TO_MAT_PARTITION:     'DATAOBJ_TO_MAT_PARTITION';
DATAOBJ_TO_PARTITION:         'DATAOBJ_TO_PARTITION';
DATAPUMP:                     'DATAPUMP';
DATA_SECURITY_REWRITE_LIMIT:  'DATA_SECURITY_REWRITE_LIMIT';
DATE:                         D A T E;
DATE_MODE:                    'DATE_MODE';
DATE_TRUNC:                   D A T E '_' T R U N C;
DAY:                          D A Y;
DAYS:                         D A Y S;
MILLISECOND:                  M I L L I S E C O N D;
MILLISECONDS:                 M I L L I S E C O N D S;
DBA:                          'DBA';
DBA_RECYCLEBIN:               'DBA_RECYCLEBIN';
DBMS_STATS:                   'DBMS_STATS';
DB_ROLE_CHANGE:               'DB_ROLE_CHANGE';
DBTIMEZONE:                   'DBTIMEZONE';
DB_UNIQUE_NAME:               'DB_UNIQUE_NAME';
DB_VERSION:                   'DB_VERSION';
DDL:                          'DDL';
DEALLOCATE:                   'DEALLOCATE';
DEBUG:                        'DEBUG';
DEBUGGER:                     'DEBUGGER';
DEC:                          'DEC';
DECIMAL:                      D E C I M A L;
DECLARE:                      'DECLARE';
DECOMPOSE:                    'DECOMPOSE';
DECORRELATE:                  'DECORRELATE';
DECR:                         'DECR';
DECREMENT:                    'DECREMENT';
DECRYPT:                      'DECRYPT';
DEDUPLICATE:                  'DEDUPLICATE';
DEFAULT:                      D E F A U L T;
DEFAULTS:                     'DEFAULTS';
DEFERRABLE:                   'DEFERRABLE';
DEFERRED:                     'DEFERRED';
DEFINED:                      'DEFINED';
DEFINE:                       'DEFINE';
DEFINER:                      'DEFINER';
DEGREE:                       'DEGREE';
DELAY:                        'DELAY';
DELEGATE:                     'DELEGATE';
DELETE_ALL:                   'DELETE_ALL';
DELETE:                       D E L E T E;
DELETEXML:                    'DELETEXML';
DEMAND:                       'DEMAND';
DENSE_RANKM:                  'DENSE_RANKM';
DEPENDENT:                    'DEPENDENT';
DEPTH:                        D E P T H;
DEQUEUE:                      'DEQUEUE';
DEREF:                        'DEREF';
DEREF_NO_REWRITE:             'DEREF_NO_REWRITE';
DESC:                         D E S C;
DESTROY:                      'DESTROY';
DETACHED:                     'DETACHED';
DETERMINES:                   'DETERMINES';
DETERMINISTIC:                'DETERMINISTIC';
DICTIONARY:                   'DICTIONARY';
DIMENSION:                    D I M E N S I O N;
DIMENSIONS:                   D I M E N S I O N S;
DIRECT_LOAD:                  'DIRECT_LOAD';
DIRECTORY:                    'DIRECTORY';
DIRECT_PATH:                  'DIRECT_PATH';
DISABLE_ALL:                  'DISABLE_ALL';
DISABLE:                      D I S A B L E;
DISABLE_PARALLEL_DML:         'DISABLE_PARALLEL_DML';
DISABLE_PRESET:               'DISABLE_PRESET';
DISABLE_RPKE:                 'DISABLE_RPKE';
DISALLOW:                     'DISALLOW';
DISASSOCIATE:                 'DISASSOCIATE';
DISCARD:                      'DISCARD';
DISCONNECT:                   'DISCONNECT';
DISK:                         'DISK';
DISKGROUP:                    'DISKGROUP';
DISKS:                        'DISKS';
DISMOUNT:                     'DISMOUNT';
DISTINCT:                     D I S T I N C T;
DISTINGUISHED:                'DISTINGUISHED';
DISTRIBUTED:                  'DISTRIBUTED';
DISTRIBUTE:                   'DISTRIBUTE';
DML:                          'DML';
DML_UPDATE:                   'DML_UPDATE';
DOCFIDELITY:                  'DOCFIDELITY';
DOCUMENT:                     'DOCUMENT';
DOMAIN_INDEX_FILTER:          'DOMAIN_INDEX_FILTER';
DOMAIN_INDEX_NO_SORT:         'DOMAIN_INDEX_NO_SORT';
DOMAIN_INDEX_SORT:            'DOMAIN_INDEX_SORT';
DOUBLE:                       D O U B L E;
DOWN:                         D O W N;
DOWNGRADE:                    'DOWNGRADE';
DRIVING_SITE:                 'DRIVING_SITE';
DROP_COLUMN:                  'DROP_COLUMN';
DROP:                         D R O P;
DROP_GROUP:                   'DROP_GROUP';
DSINTERVAL_UNCONSTRAINED:     'DSINTERVAL_UNCONSTRAINED';
DST_UPGRADE_INSERT_CONV:      'DST_UPGRADE_INSERT_CONV';
DUMP:                         'DUMP';
DUPLICATE:                    'DUPLICATE';
DV:                           'DV';
DYNAMIC:                      'DYNAMIC';
DYNAMIC_SAMPLING:             'DYNAMIC_SAMPLING';
DYNAMIC_SAMPLING_EST_CDN:     'DYNAMIC_SAMPLING_EST_CDN';
EACH:                         'EACH';
EDITIONABLE:                  'EDITIONABLE';
EDITION:                      'EDITION';
EDITIONING:                   'EDITIONING';
EDITIONS:                     'EDITIONS';
ELEMENT:                      E L E M E N T;
ELIM_GROUPBY:                 'ELIM_GROUPBY';
ELIMINATE_JOIN:               'ELIMINATE_JOIN';
ELIMINATE_OBY:                'ELIMINATE_OBY';
ELIMINATE_OUTER_JOIN:         'ELIMINATE_OUTER_JOIN';
ELSE:                         E L S E;
ELSIF:                        'ELSIF';
EM:                           'EM';
EMPTY_BLOB:                   'EMPTY_BLOB';
EMPTY_CLOB:                   'EMPTY_CLOB';
EMPTY:                        'EMPTY';
ENABLE_ALL:                   'ENABLE_ALL';
ENABLE:                       'ENABLE';
ENABLE_PARALLEL_DML:          'ENABLE_PARALLEL_DML';
ENABLE_PRESET:                'ENABLE_PRESET';
ENCODING:                     'ENCODING';
ENCRYPT:                      E N C R Y P T;
ENCRYPTION:                   'ENCRYPTION';
END:                          E N D;
END_OUTLINE_DATA:             'END_OUTLINE_DATA';
ENFORCED:                     'ENFORCED';
ENFORCE:                      'ENFORCE';
ENQUEUE:                      'ENQUEUE';
ENTERPRISE:                   'ENTERPRISE';
ENTITYESCAPING:               'ENTITYESCAPING';
ENTRY:                        'ENTRY';
EQUIPART:                     'EQUIPART';
ERR:                          'ERR';
ERROR_ARGUMENT:               'ERROR_ARGUMENT';
ERROR:                        'ERROR';
ERROR_ON_OVERLAP_TIME:        'ERROR_ON_OVERLAP_TIME';
ERRORS:                       'ERRORS';
ESCAPE:                       E S C A P E;
ESTIMATE:                     'ESTIMATE';
EVAL:                         'EVAL';
EVALNAME:                     'EVALNAME';
EVALUATE:                     'EVALUATE';
EVALUATION:                   'EVALUATION';
EVENTS:                       'EVENTS';
EVERY:                        'EVERY';
EXCEPT:                       E X C E P T;
EXCEPTION:                    'EXCEPTION';
EXCEPTION_INIT:               'EXCEPTION_INIT';
EXCEPTIONS:                   'EXCEPTIONS';
EXCHANGE:                     'EXCHANGE';
EXCLUDE:                      'EXCLUDE';
EXCLUDING:                    'EXCLUDING';
EXCLUSIVE:                    'EXCLUSIVE';
EXECUTE:                      'EXECUTE';
EXEMPT:                       'EXEMPT';
EXISTING:                     'EXISTING';
EXISTS:                       E X I S T S;
EXISTSNODE:                   'EXISTSNODE';
EXIT:                         'EXIT';
EXPAND_GSET_TO_UNION:         'EXPAND_GSET_TO_UNION';
EXPAND_TABLE:                 'EXPAND_TABLE';
EXP:                          E X P;
EXPIRE:                       'EXPIRE';
EXPLAIN:                      E X P L A I N;
EXPLOSION:                    'EXPLOSION';
EXPORT:                       'EXPORT';
EXPR_CORR_CHECK:              'EXPR_CORR_CHECK';
EXPRESS:                      'EXPRESS';
EXTENDS:                      'EXTENDS';
EXTENT:                       'EXTENT';
EXTENTS:                      'EXTENTS';
EXTERNAL:                     'EXTERNAL';
EXTERNALLY:                   'EXTERNALLY';
EXTRACTCLOBXML:               'EXTRACTCLOBXML';
EXTRACT:                      E X T R A C T;
EXTRACTVALUE:                 'EXTRACTVALUE';
EXTRA:                        'EXTRA';
FACILITY:                     'FACILITY';
FACT:                         'FACT';
FACTOR:                       'FACTOR';
FACTORIZE_JOIN:               'FACTORIZE_JOIN';
FAILED:                       'FAILED';
FAILED_LOGIN_ATTEMPTS:        'FAILED_LOGIN_ATTEMPTS';
FAILGROUP:                    'FAILGROUP';
FAILOVER:                     'FAILOVER';
FAILURE:                      'FAILURE';
FALSE:                        F A L S E;
FAMILY:                       'FAMILY';
FAR:                          'FAR';
FAST:                         'FAST';
FASTSTART:                    'FASTSTART';
FBTSCAN:                      'FBTSCAN';
FEATURE_DETAILS:              'FEATURE_DETAILS';
FEATURE_ID:                   'FEATURE_ID';
FEATURE_SET:                  'FEATURE_SET';
FEATURE_VALUE:                'FEATURE_VALUE';
FETCH:                        F E T C H;
FILE:                         'FILE';
FILE_NAME_CONVERT:            'FILE_NAME_CONVERT';
FILESYSTEM_LIKE_LOGGING:      'FILESYSTEM_LIKE_LOGGING';
FILTER:                       'FILTER';
FINAL:                        'FINAL';
FINE:                         'FINE';
FINISH:                       'FINISH';
FIRST:                        F I R S T;
FIRSTM:                       'FIRSTM';
FIRST_ROWS:                   'FIRST_ROWS';
FIRST_VALUE:                  F I R S T '_' V A L U E;
FIXED_VIEW_DATA:              'FIXED_VIEW_DATA';
FLAGGER:                      'FLAGGER';
FLASHBACK:                    'FLASHBACK';
FLASH_CACHE:                  'FLASH_CACHE';
FLOAT:                        F L O A T;
FLOB:                         'FLOB';
FLOOR:                        F L O O R;
FLUSH:                        'FLUSH';
FOLDER:                       'FOLDER';
FOLLOWING:                    'FOLLOWING';
FOLLOWS:                      'FOLLOWS';
FORALL:                       'FORALL';
FORCE:                        'FORCE';
FORCE_XML_QUERY_REWRITE:      'FORCE_XML_QUERY_REWRITE';
FOREIGN:                      F O R E I G N;
FOREVER:                      'FOREVER';
FOR:                          F O R;
FORMAT:                       F O R M A T;
FORWARD:                      'FORWARD';
FRAGMENT_NUMBER:              'FRAGMENT_NUMBER';
FREELIST:                     'FREELIST';
FREELISTS:                    'FREELISTS';
FREEPOOLS:                    'FREEPOOLS';
FRESH:                        'FRESH';
FROM:                         F R O M;
FROM_TZ:                      'FROM_TZ';
FULL:                         F U L L;
FULL_OUTER_JOIN_TO_OUTER:     'FULL_OUTER_JOIN_TO_OUTER';
FUNCTION:                     'FUNCTION';
FUNCTIONS:                    'FUNCTIONS';
GATHER_OPTIMIZER_STATISTICS:  'GATHER_OPTIMIZER_STATISTICS';
GATHER_PLAN_STATISTICS:       'GATHER_PLAN_STATISTICS';
GBY_CONC_ROLLUP:              'GBY_CONC_ROLLUP';
GBY_PUSHDOWN:                 'GBY_PUSHDOWN';
GENERATED:                    'GENERATED';
GET:                          'GET';
GET_YEAR:                      G E T [_]? Y E A R;
GET_MONTH:                     G E T [_]? M O N T H;
GET_DAY:                       G E T [_]? D A Y;
GET_HOUR:                      G E T [_]? D A Y;
GET_MINUTE:                    G E T [_]? M I N U T E;
GET_SECOND:                    G E T [_]? S E C O N D;
ADD_YEARS:                     A D D [_]? Y E A R S;
ADD_MONTHS:                    A D D [_]? M O N T H S;
ADD_DAYS:                      A D D [_]? D A Y S;
ADD_HOURS:                     A D D [_]? D A Y S;
ADD_MINUTES:                   A D D [_]? M I N U T E S;
ADD_SECONDS:                   A D D [_]? S E C O N D S;
GLOBAL:                       'GLOBAL';
GLOBALLY:                     'GLOBALLY';
GLOBAL_NAME:                  'GLOBAL_NAME';
GLOBAL_TOPIC_ENABLED:         'GLOBAL_TOPIC_ENABLED';
GOTO:                         'GOTO';
GRANT:                        'GRANT';
GROUP_BY:                     G R O U P '_' B Y;
GROUP:                        G R O U P;
GROUP_ID:                     'GROUP_ID';
GROUPING:                     G R O U P I N G;
GROUPING_ID:                  G R O U P I N G '_' I D;
GROUPS:                       'GROUPS';
GUARANTEED:                   'GUARANTEED';
GUARANTEE:                    'GUARANTEE';
GUARD:                        'GUARD';
GUID:                         G U I D;
HASH_AJ:                      'HASH_AJ';
HASH:                         'HASH';
HASHKEYS:                     'HASHKEYS';
HASH_SJ:                      'HASH_SJ';
HAVING:                       H A V I N G;
HEADER:                       'HEADER';
HEAP:                         'HEAP';
HELP:                         'HELP';
HEXTORAW:                     'HEXTORAW';
HEXTOREF:                     'HEXTOREF';
HIDDEN_KEYWORD:               'HIDDEN';
HIDE:                         'HIDE';
HIERARCHY:                    'HIERARCHY';
HIGH:                         'HIGH';
HINTSET_BEGIN:                'HINTSET_BEGIN';
HINTSET_END:                  'HINTSET_END';
HOT:                          'HOT';
HOUR:                         H O U R;
HOURS:                        H O U R S;
HWM_BROKERED:                 'HWM_BROKERED';
HYBRID:                       'HYBRID';
IDENTIFIED:                   'IDENTIFIED';
IDENTIFIER:                   'IDENTIFIER';
IDENTITY:                     'IDENTITY';
IDGENERATORS:                 'IDGENERATORS';
ID:                           'ID';
IDLE_TIME:                    'IDLE_TIME';
IF:                           I F;
IFNULL:                       I F N U L L;
IGNORE:                       I G N O R E;
IGNORE_OPTIM_EMBEDDED_HINTS:  'IGNORE_OPTIM_EMBEDDED_HINTS';
IGNORE_ROW_ON_DUPKEY_INDEX:   'IGNORE_ROW_ON_DUPKEY_INDEX';
IGNORE_WHERE_CLAUSE:          'IGNORE_WHERE_CLAUSE';
ILM:                          'ILM';
IMMEDIATE:                    'IMMEDIATE';
IMPACT:                       'IMPACT';
IMPORT:                       'IMPORT';
INACTIVE:                     'INACTIVE';
INCLUDE:                      I N C L U D E;
INCLUDE_VERSION:              'INCLUDE_VERSION';
INCLUDING:                    'INCLUDING';
INCREMENTAL:                  'INCREMENTAL';
INCREMENT:                    'INCREMENT';
INCR:                         'INCR';
INDENT:                       'INDENT';
INDEX_ASC:                    'INDEX_ASC';
INDEX_COMBINE:                'INDEX_COMBINE';
INDEX_DESC:                   'INDEX_DESC';
INDEXED:                      'INDEXED';
INDEXES:                      'INDEXES';
INDEX_FFS:                    'INDEX_FFS';
INDEX_FILTER:                 'INDEX_FILTER';
INDEX:                        I N D E X;
INDEXING:                     'INDEXING';
INDEX_JOIN:                   'INDEX_JOIN';
INDEX_ROWS:                   'INDEX_ROWS';
INDEX_RRS:                    'INDEX_RRS';
INDEX_RS_ASC:                 'INDEX_RS_ASC';
INDEX_RS_DESC:                'INDEX_RS_DESC';
INDEX_RS:                     'INDEX_RS';
INDEX_SCAN:                   'INDEX_SCAN';
INDEX_SKIP_SCAN:              'INDEX_SKIP_SCAN';
INDEX_SS_ASC:                 'INDEX_SS_ASC';
INDEX_SS_DESC:                'INDEX_SS_DESC';
INDEX_SS:                     'INDEX_SS';
INDEX_STATS:                  'INDEX_STATS';
INDEXTYPE:                    'INDEXTYPE';
INDEXTYPES:                   'INDEXTYPES';
INDICATOR:                    'INDICATOR';
INDICES:                      'INDICES';
INFINITE:                     'INFINITE';
INFORMATIONAL:                'INFORMATIONAL';
INHERIT:                      'INHERIT';
IN:                           I N;
INITCAP:                      'INITCAP';
INITIAL:                      I N I T I A L;
INITIALIZED:                  'INITIALIZED';
INITIALLY:                    'INITIALLY';
INITRANS:                     'INITRANS';
INLINE:                       'INLINE';
INLINE_XMLTYPE_NT:            'INLINE_XMLTYPE_NT';
INMEMORY:                     'INMEMORY';
IN_MEMORY_METADATA:           'IN_MEMORY_METADATA';
INMEMORY_PRUNING:             'INMEMORY_PRUNING';
INNER:                        I N N E R;
INOUT:                        'INOUT';
INPLACE:                      'INPLACE';
INSERTCHILDXMLAFTER:          'INSERTCHILDXMLAFTER';
INSERTCHILDXMLBEFORE:         'INSERTCHILDXMLBEFORE';
INSERTCHILDXML:               'INSERTCHILDXML';
INSERT:                       I N S E R T;
INSERTXMLAFTER:               'INSERTXMLAFTER';
INSERTXMLBEFORE:              'INSERTXMLBEFORE';
INSTANCE:                     'INSTANCE';
INSTANCES:                    'INSTANCES';
INSTANTIABLE:                 'INSTANTIABLE';
INSTANTLY:                    'INSTANTLY';
INSTEAD:                      'INSTEAD';
INSTR2:                       'INSTR2';
INSTR4:                       'INSTR4';
INSTRB:                       'INSTRB';
INSTRC:                       'INSTRC';
INSTR:                        'INSTR';
INTEGER:                      I N T E G E R;
INTERLEAVED:                  'INTERLEAVED';
INTERMEDIATE:                 'INTERMEDIATE';
INTERNAL_CONVERT:             'INTERNAL_CONVERT';
INTERNAL_USE:                 'INTERNAL_USE';
INTERPRETED:                  'INTERPRETED';
INTERSECT:                    I N T E R S E C T;
INTERVAL:                     I N T E R V A L;
INT:                          I N T;
INTO:                         I N T O;
INVALIDATE:                   'INVALIDATE';
INVISIBLE:                    'INVISIBLE';
IN_XQUERY:                    'IN_XQUERY';
IS:                           I S;
ISOLATION:                    'ISOLATION';
ISOLATION_LEVEL:              'ISOLATION_LEVEL';
ITERATE:                      'ITERATE';
ITERATION_NUMBER:             'ITERATION_NUMBER';
JAVA:                         'JAVA';
JOB:                          'JOB';
JOIN:                         J O I N;
JSON_ARRAYAGG:                'JSON_ARRAYAGG';
JSON_ARRAY:                   'JSON_ARRAY';
JSON_EQUAL:                   'JSON_EQUAL';
JSON_EXISTS2:                 'JSON_EXISTS2';
JSON_EXISTS:                  'JSON_EXISTS';
JSONGET:                      'JSONGET';
JSON:                         J S O N;
JSON_OBJECTAGG:               'JSON_OBJECTAGG';
JSON_OBJECT:                  'JSON_OBJECT';
JSONPARSE:                    'JSONPARSE';
JSON_QUERY:                   'JSON_QUERY';
JSON_SERIALIZE:               'JSON_SERIALIZE';
JSON_TABLE:                   'JSON_TABLE';
JSON_TEXTCONTAINS2:           'JSON_TEXTCONTAINS2';
JSON_TEXTCONTAINS:            'JSON_TEXTCONTAINS';
JSON_VALUE:                   'JSON_VALUE';
KEEP_DUPLICATES:              'KEEP_DUPLICATES';
KEEP:                         K E E P;
KERBEROS:                     'KERBEROS';
KEY:                          K E Y;
KEY_LENGTH:                   'KEY_LENGTH';
KEYSIZE:                      'KEYSIZE';
KEYS:                         'KEYS';
KEYSTORE:                     'KEYSTORE';
KILL:                         'KILL';
LABEL:                        'LABEL';
LANGUAGE:                     'LANGUAGE';
LAST_DAY:                     'LAST_DAY';
LAST:                         L A S T;
LAST_VALUE:                   L A S T '_' V A L U E;
LATERAL:                      'LATERAL';
LAX:                          'LAX';
LAYER:                        'LAYER';
LDAP_REGISTRATION_ENABLED:    'LDAP_REGISTRATION_ENABLED';
LDAP_REGISTRATION:            'LDAP_REGISTRATION';
LDAP_REG_SYNC_INTERVAL:       'LDAP_REG_SYNC_INTERVAL';
LEADING:                      L E A D I N G;
LEFT:                         L E F T;
LENGTH2:                      'LENGTH2';
LENGTH4:                      'LENGTH4';
LENGTHB:                      'LENGTHB';
LENGTHC:                      'LENGTHC';
LENGTH:                       L E N G T H;
LESS:                         'LESS';
LEVEL:                        'LEVEL';
LEVELS:                       'LEVELS';
LIBRARY:                      'LIBRARY';
LIFECYCLE:                    'LIFECYCLE';
LIFE:                         'LIFE';
LIFETIME:                     'LIFETIME';
LIKE2:                        'LIKE2';
LIKE4:                        'LIKE4';
LIKEC:                        'LIKEC';
LIKE_EXPAND:                  'LIKE_EXPAND';
LIKE:                         L I K E;
LIMIT:                        L I M I T;
LINEAR:                       'LINEAR';
LINK:                         L I N K;
LIST:                         'LIST';
LN:                           L N;
LNNVL:                        'LNNVL';
LOAD:                         'LOAD';
LOB:                          'LOB';
LOBNVL:                       'LOBNVL';
LOBS:                         'LOBS';
LOCAL_INDEXES:                'LOCAL_INDEXES';
LOCAL:                        'LOCAL';
LOCALTIME:                    'LOCALTIME';
LOCALTIMESTAMP:               'LOCALTIMESTAMP';
LOCATION:                     'LOCATION';
LOCATOR:                      'LOCATOR';
LOCKED:                       L O C K E D;
LOCKING:                      'LOCKING';
LOCK:                         'LOCK';
LOGFILE:                      'LOGFILE';
LOGFILES:                     'LOGFILES';
LOGGING:                      'LOGGING';
LOGICAL:                      'LOGICAL';
LOGICAL_READS_PER_CALL:       'LOGICAL_READS_PER_CALL';
LOGICAL_READS_PER_SESSION:    'LOGICAL_READS_PER_SESSION';
LOG:                          L O G;
LOG10:                        L O G '1' '0';
LOGMINING:                    'LOGMINING';
LOGOFF:                       'LOGOFF';
LOGON:                        'LOGON';
LOG_READ_ONLY_VIOLATIONS:     'LOG_READ_ONLY_VIOLATIONS';
LONG:                         L O N G;
LOOP:                         'LOOP';
LOWER:                        L O W E R;
LOW:                          'LOW';
LPAD:                         (L P A D) | (P A D [_] L E F T);
LTRIM:                        (L T R I M) | (T R I M [_] S T A R T);
MAIN:                         'MAIN';
MAKE_REF:                     'MAKE_REF';
MANAGED:                      'MANAGED';
MANAGE:                       'MANAGE';
MANAGEMENT:                   'MANAGEMENT';
MANAGER:                      'MANAGER';
MANUAL:                       'MANUAL';
MAP:                          'MAP';
MAPPING:                      'MAPPING';
MASTER:                       'MASTER';
MATCHED:                      M A T C H E D;
MATCHES:                      'MATCHES';
MATCH:                        'MATCH';
MATCH_NUMBER:                 'MATCH_NUMBER';
MATCH_RECOGNIZE:              'MATCH_RECOGNIZE';
MATERIALIZED:                 'MATERIALIZED';
MATERIALIZE:                  'MATERIALIZE';
MAXARCHLOGS:                  'MAXARCHLOGS';
MAXDATAFILES:                 'MAXDATAFILES';
MAXEXTENTS:                   'MAXEXTENTS';
MAXIMIZE:                     'MAXIMIZE';
MAXINSTANCES:                 'MAXINSTANCES';
MAXLOGFILES:                  'MAXLOGFILES';
MAXLOGHISTORY:                'MAXLOGHISTORY';
MAXLOGMEMBERS:                'MAXLOGMEMBERS';
MAX_SHARED_TEMP_SIZE:         'MAX_SHARED_TEMP_SIZE';
MAXSIZE:                      'MAXSIZE';
MAXTRANS:                     'MAXTRANS';
MAXVALUE:                     M A X V A L U E;
MEASURE:                      M E A S U R E;
MEASURES:                     M E A S U R E S;
MEDIUM:                       'MEDIUM';
MEMBER:                       'MEMBER';
MEMCOMPRESS:                  'MEMCOMPRESS';
MEMORY:                       'MEMORY';
MERGEACTIONS:                 'MERGE$ACTIONS';
MERGE_AJ:                     'MERGE_AJ';
MERGE_CONST_ON:               'MERGE_CONST_ON';
MERGE:                        M E R G E;
MERGE_SJ:                     'MERGE_SJ';
METADATA:                     'METADATA';
METHOD:                       'METHOD';
MIGRATE:                      'MIGRATE';
MIGRATION:                    'MIGRATION';
MINEXTENTS:                   'MINEXTENTS';
MINIMIZE:                     'MINIMIZE';
MINIMUM:                      'MINIMUM';
MINING:                       'MINING';
MINUS:                        M I N U S;
MINUS_NULL:                   'MINUS_NULL';
MINUTE:                       M I N U T E;
MINUTES:                      M I N U T E S;
MINVALUE:                     M I N V A L U E;
MIRRORCOLD:                   'MIRRORCOLD';
MIRRORHOT:                    'MIRRORHOT';
MIRROR:                       'MIRROR';
MLSLABEL:                     'MLSLABEL';
MODEL_COMPILE_SUBQUERY:       'MODEL_COMPILE_SUBQUERY';
MODEL_DONTVERIFY_UNIQUENESS:  'MODEL_DONTVERIFY_UNIQUENESS';
MODEL_DYNAMIC_SUBQUERY:       'MODEL_DYNAMIC_SUBQUERY';
MODEL_MIN_ANALYSIS:           'MODEL_MIN_ANALYSIS';
MODEL:                        M O D E L;
MODEL_NB:                     'MODEL_NB';
MODEL_NO_ANALYSIS:            'MODEL_NO_ANALYSIS';
MODEL_PBY:                    'MODEL_PBY';
MODEL_PUSH_REF:               'MODEL_PUSH_REF';
MODEL_SV:                     'MODEL_SV';
MODE:                         'MODE';
MODIFICATION:                 'MODIFICATION';
MODIFY_COLUMN_TYPE:           'MODIFY_COLUMN_TYPE';
MODIFY:                       'MODIFY';
MOD:                          M O D;
MODULE:                       'MODULE';
MONITORING:                   'MONITORING';
MONITOR:                      'MONITOR';
MONTH:                        M O N T H;
MONTHS_BETWEEN:               'MONTHS_BETWEEN';
MONTHS:                       M O N T H S;
MOUNT:                        'MOUNT';
MOUNTPATH:                    'MOUNTPATH';
MOVEMENT:                     'MOVEMENT';
MOVE:                         'MOVE';
MULTIDIMENSIONAL:             'MULTIDIMENSIONAL';
MULTISET:                     M U L T I S E T;
MV_MERGE:                     'MV_MERGE';
NAMED:                        'NAMED';
NAME:                         N A M E;
NAMESPACE:                    'NAMESPACE';
NAN_:                         'NAN'; // Avoiding conflicts with math.h NAN macros
NANVL:                        'NANVL';
NATIONAL:                     'NATIONAL';
NATIVE_FULL_OUTER_JOIN:       'NATIVE_FULL_OUTER_JOIN';
NATIVE:                       'NATIVE';
NATURAL:                      'NATURAL';
NATURALN:                     'NATURALN';
NAV:                          N A V;
NCHAR_CS:                     'NCHAR_CS';
NCHAR:                        N C H A R;
NCHR:                         'NCHR';
NCLOB:                        N C L O B;
NEEDED:                       'NEEDED';
NEG:                          'NEG';
NESTED:                       'NESTED';
NESTED_TABLE_FAST_INSERT:     'NESTED_TABLE_FAST_INSERT';
NESTED_TABLE_GET_REFS:        'NESTED_TABLE_GET_REFS';
NESTED_TABLE_ID:              'NESTED_TABLE_ID';
NESTED_TABLE_SET_REFS:        'NESTED_TABLE_SET_REFS';
NESTED_TABLE_SET_SETID:       'NESTED_TABLE_SET_SETID';
NETWORK:                      'NETWORK';
NEVER:                        'NEVER';
NEW:                          'NEW';
NEW_TIME:                     'NEW_TIME';
NEXT_DAY:                     'NEXT_DAY';
NEXT:                         N E X T;
NL_AJ:                        'NL_AJ';
NLJ_BATCHING:                 'NLJ_BATCHING';
NLJ_INDEX_FILTER:             'NLJ_INDEX_FILTER';
NLJ_INDEX_SCAN:               'NLJ_INDEX_SCAN';
NLJ_PREFETCH:                 'NLJ_PREFETCH';
NLS_CALENDAR:                 'NLS_CALENDAR';
NLS_CHARACTERSET:             'NLS_CHARACTERSET';
NLS_CHARSET_DECL_LEN:         'NLS_CHARSET_DECL_LEN';
NLS_CHARSET_ID:               'NLS_CHARSET_ID';
NLS_CHARSET_NAME:             'NLS_CHARSET_NAME';
NLS_COMP:                     'NLS_COMP';
NLS_CURRENCY:                 'NLS_CURRENCY';
NLS_DATE_FORMAT:              'NLS_DATE_FORMAT';
NLS_DATE_LANGUAGE:            'NLS_DATE_LANGUAGE';
NLS_INITCAP:                  'NLS_INITCAP';
NLS_ISO_CURRENCY:             'NLS_ISO_CURRENCY';
NL_SJ:                        'NL_SJ';
NLS_LANG:                     'NLS_LANG';
NLS_LANGUAGE:                 'NLS_LANGUAGE';
NLS_LENGTH_SEMANTICS:         'NLS_LENGTH_SEMANTICS';
NLS_LOWER:                    'NLS_LOWER';
NLS_NCHAR_CONV_EXCP:          'NLS_NCHAR_CONV_EXCP';
NLS_NUMERIC_CHARACTERS:       'NLS_NUMERIC_CHARACTERS';
NLS_SORT:                     'NLS_SORT';
NLSSORT:                      'NLSSORT';
NLS_SPECIAL_CHARS:            'NLS_SPECIAL_CHARS';
NLS_TERRITORY:                'NLS_TERRITORY';
NLS_UPPER:                    'NLS_UPPER';
NO_ACCESS:                    'NO_ACCESS';
NO_ADAPTIVE_PLAN:             'NO_ADAPTIVE_PLAN';
NO_ANSI_REARCH:               'NO_ANSI_REARCH';
NOAPPEND:                     'NOAPPEND';
NOARCHIVELOG:                 'NOARCHIVELOG';
NOAUDIT:                      'NOAUDIT';
NO_AUTO_REOPTIMIZE:           'NO_AUTO_REOPTIMIZE';
NO_BASETABLE_MULTIMV_REWRITE: 'NO_BASETABLE_MULTIMV_REWRITE';
NO_BATCH_TABLE_ACCESS_BY_ROWID: 'NO_BATCH_TABLE_ACCESS_BY_ROWID';
NO_BIND_AWARE:                'NO_BIND_AWARE';
NO_BUFFER:                    'NO_BUFFER';
NOCACHE:                      'NOCACHE';
NO_CARTESIAN:                 'NO_CARTESIAN';
NO_CHECK_ACL_REWRITE:         'NO_CHECK_ACL_REWRITE';
NO_CLUSTER_BY_ROWID:          'NO_CLUSTER_BY_ROWID';
NO_CLUSTERING:                'NO_CLUSTERING';
NO_COALESCE_SQ:               'NO_COALESCE_SQ';
NO_COMMON_DATA:               'NO_COMMON_DATA';
NOCOMPRESS:                   'NOCOMPRESS';
NO_CONNECT_BY_CB_WHR_ONLY:    'NO_CONNECT_BY_CB_WHR_ONLY';
NO_CONNECT_BY_COMBINE_SW:     'NO_CONNECT_BY_COMBINE_SW';
NO_CONNECT_BY_COST_BASED:     'NO_CONNECT_BY_COST_BASED';
NO_CONNECT_BY_ELIM_DUPS:      'NO_CONNECT_BY_ELIM_DUPS';
NO_CONNECT_BY_FILTERING:      'NO_CONNECT_BY_FILTERING';
NOCOPY:                       'NOCOPY';
NO_COST_XML_QUERY_REWRITE:    'NO_COST_XML_QUERY_REWRITE';
NO_CPU_COSTING:               'NO_CPU_COSTING';
NOCPU_COSTING:                'NOCPU_COSTING';
NOCYCLE:                      N O C Y C L E;
NO_DATA_SECURITY_REWRITE:     'NO_DATA_SECURITY_REWRITE';
NO_DECORRELATE:               'NO_DECORRELATE';
NODELAY:                      'NODELAY';
NO_DOMAIN_INDEX_FILTER:       'NO_DOMAIN_INDEX_FILTER';
NO_DST_UPGRADE_INSERT_CONV:   'NO_DST_UPGRADE_INSERT_CONV';
NO_ELIM_GROUPBY:              'NO_ELIM_GROUPBY';
NO_ELIMINATE_JOIN:            'NO_ELIMINATE_JOIN';
NO_ELIMINATE_OBY:             'NO_ELIMINATE_OBY';
NO_ELIMINATE_OUTER_JOIN:      'NO_ELIMINATE_OUTER_JOIN';
NOENTITYESCAPING:             'NOENTITYESCAPING';
NO_EXPAND_GSET_TO_UNION:      'NO_EXPAND_GSET_TO_UNION';
NO_EXPAND:                    'NO_EXPAND';
NO_EXPAND_TABLE:              'NO_EXPAND_TABLE';
NO_FACT:                      'NO_FACT';
NO_FACTORIZE_JOIN:            'NO_FACTORIZE_JOIN';
NO_FILTERING:                 'NO_FILTERING';
NOFORCE:                      'NOFORCE';
NO_FULL_OUTER_JOIN_TO_OUTER:  'NO_FULL_OUTER_JOIN_TO_OUTER';
NO_GATHER_OPTIMIZER_STATISTICS: 'NO_GATHER_OPTIMIZER_STATISTICS';
NO_GBY_PUSHDOWN:              'NO_GBY_PUSHDOWN';
NOGUARANTEE:                  'NOGUARANTEE';
NO_INDEX_FFS:                 'NO_INDEX_FFS';
NO_INDEX:                     'NO_INDEX';
NO_INDEX_SS:                  'NO_INDEX_SS';
NO_INMEMORY:                  'NO_INMEMORY';
NO_INMEMORY_PRUNING:          'NO_INMEMORY_PRUNING';
NOKEEP:                       'NOKEEP';
NO_LOAD:                      'NO_LOAD';
NOLOCAL:                      'NOLOCAL';
NOLOGGING:                    'NOLOGGING';
NOMAPPING:                    'NOMAPPING';
NOMAXVALUE:                   'NOMAXVALUE';
NO_MERGE:                     'NO_MERGE';
NOMINIMIZE:                   'NOMINIMIZE';
NOMINVALUE:                   'NOMINVALUE';
NO_MODEL_PUSH_REF:            'NO_MODEL_PUSH_REF';
NO_MONITORING:                'NO_MONITORING';
NOMONITORING:                 'NOMONITORING';
NO_MONITOR:                   'NO_MONITOR';
NO_MULTIMV_REWRITE:           'NO_MULTIMV_REWRITE';
NO_NATIVE_FULL_OUTER_JOIN:    'NO_NATIVE_FULL_OUTER_JOIN';
NONBLOCKING:                  'NONBLOCKING';
NONEDITIONABLE:               'NONEDITIONABLE';
NONE:                         'NONE';
NO_NLJ_BATCHING:              'NO_NLJ_BATCHING';
NO_NLJ_PREFETCH:              'NO_NLJ_PREFETCH';
NO:                           'NO';
NONSCHEMA:                    'NONSCHEMA';
NO_OBJECT_LINK:               'NO_OBJECT_LINK';
NOORDER:                      'NOORDER';
NO_ORDER_ROLLUPS:             'NO_ORDER_ROLLUPS';
NO_OUTER_JOIN_TO_ANTI:        'NO_OUTER_JOIN_TO_ANTI';
NO_OUTER_JOIN_TO_INNER:       'NO_OUTER_JOIN_TO_INNER';
NOOVERRIDE:                   'NOOVERRIDE';
NO_PARALLEL_INDEX:            'NO_PARALLEL_INDEX';
NOPARALLEL_INDEX:             'NOPARALLEL_INDEX';
NO_PARALLEL:                  'NO_PARALLEL';
NOPARALLEL:                   'NOPARALLEL';
NO_PARTIAL_COMMIT:            'NO_PARTIAL_COMMIT';
NO_PARTIAL_JOIN:              'NO_PARTIAL_JOIN';
NO_PARTIAL_ROLLUP_PUSHDOWN:   'NO_PARTIAL_ROLLUP_PUSHDOWN';
NOPARTITION:                  'NOPARTITION';
NO_PLACE_DISTINCT:            'NO_PLACE_DISTINCT';
NO_PLACE_GROUP_BY:            'NO_PLACE_GROUP_BY';
NO_PQ_CONCURRENT_UNION:       'NO_PQ_CONCURRENT_UNION';
NO_PQ_MAP:                    'NO_PQ_MAP';
NO_PQ_REPLICATE:              'NO_PQ_REPLICATE';
NO_PQ_SKEW:                   'NO_PQ_SKEW';
NO_PRUNE_GSETS:               'NO_PRUNE_GSETS';
NO_PULL_PRED:                 'NO_PULL_PRED';
NO_PUSH_PRED:                 'NO_PUSH_PRED';
NO_PUSH_SUBQ:                 'NO_PUSH_SUBQ';
NO_PX_FAULT_TOLERANCE:        'NO_PX_FAULT_TOLERANCE';
NO_PX_JOIN_FILTER:            'NO_PX_JOIN_FILTER';
NO_QKN_BUFF:                  'NO_QKN_BUFF';
NO_QUERY_TRANSFORMATION:      'NO_QUERY_TRANSFORMATION';
NO_REF_CASCADE:               'NO_REF_CASCADE';
NORELOCATE:                   'NORELOCATE';
NORELY:                       'NORELY';
NOREPAIR:                     'NOREPAIR';
NOREPLAY:                     'NOREPLAY';
NORESETLOGS:                  'NORESETLOGS';
NO_RESULT_CACHE:              'NO_RESULT_CACHE';
NOREVERSE:                    'NOREVERSE';
NO_REWRITE:                   'NO_REWRITE';
NOREWRITE:                    'NOREWRITE';
NORMAL:                       'NORMAL';
NO_ROOT_SW_FOR_LOCAL:         'NO_ROOT_SW_FOR_LOCAL';
NOROWDEPENDENCIES:            'NOROWDEPENDENCIES';
NOSCHEMACHECK:                'NOSCHEMACHECK';
NOSEGMENT:                    'NOSEGMENT';
NO_SEMIJOIN:                  'NO_SEMIJOIN';
NO_SEMI_TO_INNER:             'NO_SEMI_TO_INNER';
NO_SET_TO_JOIN:               'NO_SET_TO_JOIN';
NOSORT:                       'NOSORT';
NO_SQL_TRANSLATION:           'NO_SQL_TRANSLATION';
NO_SQL_TUNE:                  'NO_SQL_TUNE';
NO_STAR_TRANSFORMATION:       'NO_STAR_TRANSFORMATION';
NO_STATEMENT_QUEUING:         'NO_STATEMENT_QUEUING';
NO_STATS_GSETS:               'NO_STATS_GSETS';
NOSTRICT:                     'NOSTRICT';
NO_SUBQUERY_PRUNING:          'NO_SUBQUERY_PRUNING';
NO_SUBSTRB_PAD:               'NO_SUBSTRB_PAD';
NO_SWAP_JOIN_INPUTS:          'NO_SWAP_JOIN_INPUTS';
NOSWITCH:                     'NOSWITCH';
NO_TABLE_LOOKUP_BY_NL:        'NO_TABLE_LOOKUP_BY_NL';
NO_TEMP_TABLE:                'NO_TEMP_TABLE';
NOTHING:                      'NOTHING';
NOTIFICATION:                 'NOTIFICATION';
NOT:                          N O T;
NO_TRANSFORM_DISTINCT_AGG:    'NO_TRANSFORM_DISTINCT_AGG';
NO_UNNEST:                    'NO_UNNEST';
NO_USE_CUBE:                  'NO_USE_CUBE';
NO_USE_HASH_AGGREGATION:      'NO_USE_HASH_AGGREGATION';
NO_USE_HASH_GBY_FOR_PUSHDOWN: 'NO_USE_HASH_GBY_FOR_PUSHDOWN';
NO_USE_HASH:                  'NO_USE_HASH';
NO_USE_INVISIBLE_INDEXES:     'NO_USE_INVISIBLE_INDEXES';
NO_USE_MERGE:                 'NO_USE_MERGE';
NO_USE_NL:                    'NO_USE_NL';
NO_USE_VECTOR_AGGREGATION:    'NO_USE_VECTOR_AGGREGATION';
NOVALIDATE:                   'NOVALIDATE';
NO_VECTOR_TRANSFORM_DIMS:     'NO_VECTOR_TRANSFORM_DIMS';
NO_VECTOR_TRANSFORM_FACT:     'NO_VECTOR_TRANSFORM_FACT';
NO_VECTOR_TRANSFORM:          'NO_VECTOR_TRANSFORM';
NOWAIT:                       N O W A I T;
NO_XDB_FASTPATH_INSERT:       'NO_XDB_FASTPATH_INSERT';
NO_XML_DML_REWRITE:           'NO_XML_DML_REWRITE';
NO_XMLINDEX_REWRITE_IN_SELECT: 'NO_XMLINDEX_REWRITE_IN_SELECT';
NO_XMLINDEX_REWRITE:          'NO_XMLINDEX_REWRITE';
NO_XML_QUERY_REWRITE:         'NO_XML_QUERY_REWRITE';
NO_ZONEMAP:                   'NO_ZONEMAP';
NTH_VALUE:                    'NTH_VALUE';
NULLIF:                       N U L L I F;
NULL:                         N U L L;
NULLS:                        N U L L S;
NUMBER:                       N U M B E R;
NUMERIC:                      N U M E R I C;
NUM_INDEX_KEYS:               'NUM_INDEX_KEYS';
NUMTODSINTERVAL:              'NUMTODSINTERVAL';
NUMTOYMINTERVAL:              'NUMTOYMINTERVAL';
NVARCHAR2:                    N V A R C H A R '2';
NVL2:                         N V L '2';
OBJECT2XML:                   'OBJECT2XML';
OBJECT:                       'OBJECT';
OBJ_ID:                       'OBJ_ID';
OBJNO:                        'OBJNO';
OBJNO_REUSE:                  'OBJNO_REUSE';
OCCURENCES:                   'OCCURENCES';
OFFLINE:                      'OFFLINE';
OFF:                          'OFF';
OFFSET:                       O F F S E T;
OF:                           O F;
OIDINDEX:                     'OIDINDEX';
OID:                          'OID';
OLAP:                         'OLAP';
OLD:                          'OLD';
OLD_PUSH_PRED:                'OLD_PUSH_PRED';
OLS:                          'OLS';
OLTP:                         'OLTP';
OMIT:                         'OMIT';
ONE:                          'ONE';
ONLINE:                       'ONLINE';
ONLY:                         O N L Y;
ON:                           O N;
OPAQUE:                       'OPAQUE';
OPAQUE_TRANSFORM:             'OPAQUE_TRANSFORM';
OPAQUE_XCANONICAL:            'OPAQUE_XCANONICAL';
OPCODE:                       'OPCODE';
OPEN:                         'OPEN';
OPERATIONS:                   'OPERATIONS';
OPERATOR:                     'OPERATOR';
OPT_ESTIMATE:                 'OPT_ESTIMATE';
OPTIMAL:                      'OPTIMAL';
OPTIMIZE:                     'OPTIMIZE';
OPTIMIZER_FEATURES_ENABLE:    'OPTIMIZER_FEATURES_ENABLE';
OPTIMIZER_GOAL:               'OPTIMIZER_GOAL';
OPTION:                       'OPTION';
OPT_PARAM:                    'OPT_PARAM';
ORA_BRANCH:                   'ORA_BRANCH';
ORA_CHECK_ACL:                'ORA_CHECK_ACL';
ORA_CHECK_PRIVILEGE:          'ORA_CHECK_PRIVILEGE';
ORA_CLUSTERING:               'ORA_CLUSTERING';
ORADATA:                      'ORADATA';
ORADEBUG:                     'ORADEBUG';
ORA_DST_AFFECTED:             'ORA_DST_AFFECTED';
ORA_DST_CONVERT:              'ORA_DST_CONVERT';
ORA_DST_ERROR:                'ORA_DST_ERROR';
ORA_GET_ACLIDS:               'ORA_GET_ACLIDS';
ORA_GET_PRIVILEGES:           'ORA_GET_PRIVILEGES';
ORA_HASH:                     'ORA_HASH';
ORA_INVOKING_USERID:          'ORA_INVOKING_USERID';
ORA_INVOKING_USER:            'ORA_INVOKING_USER';
ORA_INVOKING_XS_USER_GUID:    'ORA_INVOKING_XS_USER_GUID';
ORA_INVOKING_XS_USER:         'ORA_INVOKING_XS_USER';
ORA_RAWCOMPARE:               'ORA_RAWCOMPARE';
ORA_RAWCONCAT:                'ORA_RAWCONCAT';
ORA_ROWSCN:                   'ORA_ROWSCN';
ORA_ROWSCN_RAW:               'ORA_ROWSCN_RAW';
ORA_ROWVERSION:               'ORA_ROWVERSION';
ORA_TABVERSION:               'ORA_TABVERSION';
ORA_WRITE_TIME:               'ORA_WRITE_TIME';
ORDERED:                      'ORDERED';
ORDERED_PREDICATES:           'ORDERED_PREDICATES';
ORDER:                        O R D E R;
ORDINALITY:                   'ORDINALITY';
OR_EXPAND:                    'OR_EXPAND';
ORGANIZATION:                 'ORGANIZATION';
OR:                           O R;
OR_PREDICATES:                'OR_PREDICATES';
OSERROR:                      'OSERROR';
OTHER:                        'OTHER';
OUTER_JOIN_TO_ANTI:           'OUTER_JOIN_TO_ANTI';
OUTER_JOIN_TO_INNER:          'OUTER_JOIN_TO_INNER';
OUTER:                        O U T E R;
OUTLINE_LEAF:                 'OUTLINE_LEAF';
OUTLINE:                      'OUTLINE';
OUT_OF_LINE:                  'OUT_OF_LINE';
OUT:                          'OUT';
OVERFLOW_NOMOVE:              'OVERFLOW_NOMOVE';
OVERFLOW_:                    'OVERFLOW'; // Avoiding conflicts with math.h OVERFLOW macros
OVERLAPS:                     'OVERLAPS';
OVER:                         O V E R;
OVERRIDING:                   'OVERRIDING';
OWNER:                        'OWNER';
OWNERSHIP:                    'OWNERSHIP';
OWN:                          'OWN';
PACKAGE:                      'PACKAGE';
PACKAGES:                     'PACKAGES';
PARALLEL_ENABLE:              'PARALLEL_ENABLE';
PARALLEL_INDEX:               'PARALLEL_INDEX';
PARALLEL:                     'PARALLEL';
PARAMETERS:                   'PARAMETERS';
PARAM:                        'PARAM';
PARENT:                       'PARENT';
PARITY:                       'PARITY';
PARTIAL_JOIN:                 'PARTIAL_JOIN';
PARTIALLY:                    'PARTIALLY';
PARTIAL:                      'PARTIAL';
PARTIAL_ROLLUP_PUSHDOWN:      'PARTIAL_ROLLUP_PUSHDOWN';
PARTITION_HASH:               'PARTITION_HASH';
PARTITION_LIST:               'PARTITION_LIST';
PARTITION:                    P A R T I T I O N;
PARTITION_RANGE:              'PARTITION_RANGE';
PARTITIONS:                   'PARTITIONS';
PARTNUMINST:                  'PART$NUM$INST';
PASSING:                      P A S S I N G;
PASSWORD_GRACE_TIME:          'PASSWORD_GRACE_TIME';
PASSWORD_LIFE_TIME:           'PASSWORD_LIFE_TIME';
PASSWORD_LOCK_TIME:           'PASSWORD_LOCK_TIME';
PASSWORD:                     'PASSWORD';
PASSWORD_REUSE_MAX:           'PASSWORD_REUSE_MAX';
PASSWORD_REUSE_TIME:          'PASSWORD_REUSE_TIME';
PASSWORD_VERIFY_FUNCTION:     'PASSWORD_VERIFY_FUNCTION';
PAST:                         'PAST';
PATCH:                        'PATCH';
PATH:                         'PATH';
PATH_PREFIX:                  'PATH_PREFIX';
PATHS:                        'PATHS';
PATTERN:                      'PATTERN';
PBL_HS_BEGIN:                 'PBL_HS_BEGIN';
PBL_HS_END:                   'PBL_HS_END';
PCTFREE:                      'PCTFREE';
PCTINCREASE:                  'PCTINCREASE';
PCTTHRESHOLD:                 'PCTTHRESHOLD';
PCTUSED:                      'PCTUSED';
PCTVERSION:                   'PCTVERSION';
PENDING:                      'PENDING';
PERCENT_FOUND:                '%FOUND';
PERCENT_ISOPEN:               '%ISOPEN';
PERCENT_NOTFOUND:             '%NOTFOUND';
PERCENT_KEYWORD:              P E R C E N T;
PERCENT_RANKM:                'PERCENT_RANKM';
PERCENT_ROWCOUNT:             '%ROWCOUNT';
PERCENT_ROWTYPE:              '%ROWTYPE';
PERCENT_TYPE:                 '%TYPE';
PERFORMANCE:                  'PERFORMANCE';
PERIOD_KEYWORD:               'PERIOD';
PERMANENT:                    'PERMANENT';
PERMISSION:                   'PERMISSION';
PERMUTE:                      'PERMUTE';
PER:                          'PER';
PFILE:                        'PFILE';
PHYSICAL:                     'PHYSICAL';
PIKEY:                        'PIKEY';
PIPELINED:                    'PIPELINED';
PIPE:                         'PIPE';
PIV_GB:                       'PIV_GB';
PIVOT:                        P I V O T;
PIV_SSF:                      'PIV_SSF';
PLACE_DISTINCT:               'PLACE_DISTINCT';
PLACE_GROUP_BY:               'PLACE_GROUP_BY';
PLAN:                         P L A N;
PLSCOPE_SETTINGS:             'PLSCOPE_SETTINGS';
PLS_INTEGER:                  'PLS_INTEGER';
PLSQL_CCFLAGS:                'PLSQL_CCFLAGS';
PLSQL_CODE_TYPE:              'PLSQL_CODE_TYPE';
PLSQL_DEBUG:                  'PLSQL_DEBUG';
PLSQL_OPTIMIZE_LEVEL:         'PLSQL_OPTIMIZE_LEVEL';
PLSQL_WARNINGS:               'PLSQL_WARNINGS';
PLUGGABLE:                    'PLUGGABLE';
POINT:                        'POINT';
POLICY:                       'POLICY';
POOL_16K:                     'POOL_16K';
POOL_2K:                      'POOL_2K';
POOL_32K:                     'POOL_32K';
POOL_4K:                      'POOL_4K';
POOL_8K:                      'POOL_8K';
POSITIVEN:                    'POSITIVEN';
POSITIVE:                     'POSITIVE';
POST_TRANSACTION:             'POST_TRANSACTION';
POWERMULTISET_BY_CARDINALITY: 'POWERMULTISET_BY_CARDINALITY';
POWERMULTISET:                'POWERMULTISET';
POWER:                        P O W (E R)?;
PQ_CONCURRENT_UNION:          'PQ_CONCURRENT_UNION';
PQ_DISTRIBUTE:                'PQ_DISTRIBUTE';
PQ_DISTRIBUTE_WINDOW:         'PQ_DISTRIBUTE_WINDOW';
PQ_FILTER:                    'PQ_FILTER';
PQ_MAP:                       'PQ_MAP';
PQ_NOMAP:                     'PQ_NOMAP';
PQ_REPLICATE:                 'PQ_REPLICATE';
PQ_SKEW:                      'PQ_SKEW';
PRAGMA:                       'PRAGMA';
PREBUILT:                     'PREBUILT';
PRECEDES:                     'PRECEDES';
PRECEDING:                    'PRECEDING';
PRECISION:                    P R E C I S I O N;
PRECOMPUTE_SUBQUERY:          'PRECOMPUTE_SUBQUERY';
PREDICATE_REORDERS:           'PREDICATE_REORDERS';
PRELOAD:                      'PRELOAD';
PREPARE:                      'PREPARE';
PRESENTNNV:                   'PRESENTNNV';
PRESENT:                      'PRESENT';
PRESENTV:                     'PRESENTV';
PRESERVE_OID:                 'PRESERVE_OID';
PRESERVE:                     'PRESERVE';
PRETTY:                       'PRETTY';
PREVIOUS:                     'PREVIOUS';
PREV:                         'PREV';
PRIMARY:                      P R I M A R Y;
PRINTBLOBTOCLOB:              'PRINTBLOBTOCLOB';
PRIORITY:                     'PRIORITY';
PRIOR:                        P R I O R;
PRIVATE:                      'PRIVATE';
PRIVATE_SGA:                  'PRIVATE_SGA';
PRIVILEGED:                   'PRIVILEGED';
PRIVILEGE:                    'PRIVILEGE';
PRIVILEGES:                   'PRIVILEGES';
PROCEDURAL:                   'PROCEDURAL';
PROCEDURE:                    'PROCEDURE';
PROCESS:                      'PROCESS';
PROFILE:                      'PROFILE';
PROGRAM:                      'PROGRAM';
PROJECT:                      'PROJECT';
PROPAGATE:                    'PROPAGATE';
PROTECTED:                    'PROTECTED';
PROTECTION:                   'PROTECTION';
PROXY:                        'PROXY';
PRUNING:                      'PRUNING';
PUBLIC:                       P U B L I C;
PULL_PRED:                    'PULL_PRED';
PURGE:                        'PURGE';
PUSH_PRED:                    'PUSH_PRED';
PUSH_SUBQ:                    'PUSH_SUBQ';
PX_FAULT_TOLERANCE:           'PX_FAULT_TOLERANCE';
PX_GRANULE:                   'PX_GRANULE';
PX_JOIN_FILTER:               'PX_JOIN_FILTER';
QB_NAME:                      'QB_NAME';
QUERY_BLOCK:                  'QUERY_BLOCK';
QUERY:                        'QUERY';
QUEUE_CURR:                   'QUEUE_CURR';
QUEUE:                        'QUEUE';
QUEUE_ROWP:                   'QUEUE_ROWP';
QUIESCE:                      'QUIESCE';
QUORUM:                       'QUORUM';
QUOTA:                        'QUOTA';
QUARTER:                      Q U A R T E R;
RAISE:                        'RAISE';
RANDOM_LOCAL:                 'RANDOM_LOCAL';
RANDOM:                       'RANDOM';
RANGE:                        'RANGE';
RANKM:                        'RANKM';
RAPIDLY:                      'RAPIDLY';
RAW:                          R A W;
RAW_SQL:                      R A W '_' S Q L;
RAWTOHEX:                     'RAWTOHEX';
RAWTONHEX:                    'RAWTONHEX';
RBA:                          'RBA';
RBO_OUTLINE:                  'RBO_OUTLINE';
RDBA:                         'RDBA';
READ:                         'READ';
READS:                        'READS';
REALM:                        'REALM';
REAL:                         R E A L;
REBALANCE:                    'REBALANCE';
REBUILD:                      'REBUILD';
RECORD:                       'RECORD';
RECORDS_PER_BLOCK:            'RECORDS_PER_BLOCK';
RECOVERABLE:                  'RECOVERABLE';
RECOVER:                      'RECOVER';
RECOVERY:                     'RECOVERY';
RECYCLEBIN:                   'RECYCLEBIN';
RECYCLE:                      'RECYCLE';
REDACTION:                    'REDACTION';
REDEFINE:                     'REDEFINE';
REDO:                         'REDO';
REDUCED:                      'REDUCED';
REDUNDANCY:                   'REDUNDANCY';
REF_CASCADE_CURSOR:           'REF_CASCADE_CURSOR';
REFERENCED:                   'REFERENCED';
REFERENCE:                    'REFERENCE';
REFERENCES:                   R E F E R E N C E S;
REFERENCING:                  'REFERENCING';
REF:                          'REF';
REFRESH:                      'REFRESH';
REFTOHEX:                     'REFTOHEX';
REGEXP_COUNT:                 'REGEXP_COUNT';
REGEXP_INSTR:                 'REGEXP_INSTR';
REGEXP_LIKE:                  'REGEXP_LIKE';
REGEXP_REPLACE:               'REGEXP_REPLACE';
REGEXP_SUBSTR:                'REGEXP_SUBSTR';
REGISTER:                     'REGISTER';
REGR_AVGX:                    'REGR_AVGX';
REGR_AVGY:                    'REGR_AVGY';
REGR_COUNT:                   'REGR_COUNT';
REGR_INTERCEPT:               'REGR_INTERCEPT';
REGR_R2:                      'REGR_R2';
REGR_SLOPE:                   'REGR_SLOPE';
REGR_SXX:                     'REGR_SXX';
REGR_SXY:                     'REGR_SXY';
REGR_SYY:                     'REGR_SYY';
REGULAR:                      'REGULAR';
REJECT:                       'REJECT';
REKEY:                        'REKEY';
RELATIONAL:                   'RELATIONAL';
RELIES_ON:                    'RELIES_ON';
RELOCATE:                     'RELOCATE';
RELY:                         'RELY';
REMAINDER:                    'REMAINDER';
REMOTE_MAPPED:                'REMOTE_MAPPED';
REMOVE:                       'REMOVE';
RENAME:                       'RENAME';
REPAIR:                       'REPAIR';
REPEAT:                       'REPEAT';
REPLACE:                      R E P L A C E;
REPLICATION:                  'REPLICATION';
REQUIRED:                     'REQUIRED';
RESETLOGS:                    'RESETLOGS';
RESET:                        'RESET';
RESIZE:                       'RESIZE';
RESOLVE:                      'RESOLVE';
RESOLVER:                     'RESOLVER';
RESOURCE:                     'RESOURCE';
RESPECT:                      R E S P E C T;
RESTART:                      'RESTART';
RESTORE_AS_INTERVALS:         'RESTORE_AS_INTERVALS';
RESTORE:                      'RESTORE';
RESTRICT_ALL_REF_CONS:        'RESTRICT_ALL_REF_CONS';
RESTRICTED:                   'RESTRICTED';
RESTRICT_REFERENCES:          'RESTRICT_REFERENCES';
RESTRICT:                     'RESTRICT';
RESULT_CACHE:                 'RESULT_CACHE';
RESULT:                       'RESULT';
RESUMABLE:                    'RESUMABLE';
RESUME:                       'RESUME';
RETENTION:                    'RETENTION';
RETRY_ON_ROW_CHANGE:          'RETRY_ON_ROW_CHANGE';
RETURNING:                    R E T U R N I N G;
RETURN:                       'RETURN';
REUSE:                        'REUSE';
REVERSE:                      'REVERSE';
REVOKE:                       'REVOKE';
REWRITE_OR_ERROR:             'REWRITE_OR_ERROR';
REWRITE:                      'REWRITE';
RIGHT:                        R I G H T;
ROLE:                         'ROLE';
ROLESET:                      'ROLESET';
ROLES:                        'ROLES';
ROLLBACK:                     'ROLLBACK';
ROLLING:                      'ROLLING';
ROLLUP:                       R O L L U P;
ROWDEPENDENCIES:              'ROWDEPENDENCIES';
ROWID_MAPPING_TABLE:          'ROWID_MAPPING_TABLE';
ROWID:                        R O W I D;
ROWIDTOCHAR:                  'ROWIDTOCHAR';
ROWIDTONCHAR:                 'ROWIDTONCHAR';
ROW_LENGTH:                   'ROW_LENGTH';
ROWNUM:                       R O W N U M;
ROW:                          R O W;
ROWS:                         R O W S;
RPAD:                         (R P A D) | (P A D [_] R I G H T);
RTRIM:                        (R T R I M) | (T R I M [_] E N D);
RULE:                         R U L E;
RULES:                        R U L E S;
RUNNING:                      'RUNNING';
SALT:                         S A L T;
SAMPLE:                       S A M P L E;
SAVE_AS_INTERVALS:            'SAVE_AS_INTERVALS';
SAVEPOINT:                    'SAVEPOINT';
SAVE:                         'SAVE';
SB4:                          'SB4';
SCALE_ROWS:                   'SCALE_ROWS';
SCALE:                        'SCALE';
SCAN_INSTANCES:               'SCAN_INSTANCES';
SCAN:                         'SCAN';
SCHEDULER:                    'SCHEDULER';
SCHEMACHECK:                  'SCHEMACHECK';
SCHEMA:                       S C H E M A;
SCN_ASCENDING:                'SCN_ASCENDING';
SCN:                          'SCN';
SCOPE:                        'SCOPE';
SCRUB:                        'SCRUB';
SD_ALL:                       'SD_ALL';
SD_INHIBIT:                   'SD_INHIBIT';
SDO_GEOM_MBR:                 'SDO_GEOM_MBR';
SD_SHOW:                      'SD_SHOW';
SEARCH:                       S E A R C H;
SECOND:                       S E C O N D;
SECONDS:                      S E C O N D S;
SECRET:                       'SECRET';
SECUREFILE_DBA:               'SECUREFILE_DBA';
SECUREFILE:                   'SECUREFILE';
SECURITY:                     'SECURITY';
SEED:                         S E E D;
SEG_BLOCK:                    'SEG_BLOCK';
SEG_FILE:                     'SEG_FILE';
SEGMENT:                      'SEGMENT';
SELECTIVITY:                  'SELECTIVITY';
SELECT:                       S E L E C T;
SELF:                         'SELF';
SEMIJOIN_DRIVER:              'SEMIJOIN_DRIVER';
SEMIJOIN:                     'SEMIJOIN';
SEMI_TO_INNER:                'SEMI_TO_INNER';
SEQUENCED:                    'SEQUENCED';
SEQUENCE:                     S E Q U E N C E;
SEQUENTIAL:                   S E Q U E N T I A L;
SERIALIZABLE:                 'SERIALIZABLE';
SERIALLY_REUSABLE:            'SERIALLY_REUSABLE';
SERIAL:                       'SERIAL';
SERVERERROR:                  'SERVERERROR';
SERVICE_NAME_CONVERT:         'SERVICE_NAME_CONVERT';
SERVICES:                     'SERVICES';
SESSION_CACHED_CURSORS:       'SESSION_CACHED_CURSORS';
SESSION:                      'SESSION';
SESSIONS_PER_USER:            'SESSIONS_PER_USER';
SESSIONTIMEZONE:              'SESSIONTIMEZONE';
SESSIONTZNAME:                'SESSIONTZNAME';
SET:                          S E T;
SETS:                         S E T S;
SETTINGS:                     'SETTINGS';
SET_TO_JOIN:                  'SET_TO_JOIN';
SEVERE:                       'SEVERE';
SHARED_POOL:                  'SHARED_POOL';
SHARED:                       'SHARED';
SHARE:                        'SHARE';
SHARING:                      'SHARING';
SHELFLIFE:                    'SHELFLIFE';
SHOW:                         'SHOW';
SHRINK:                       'SHRINK';
SHUTDOWN:                     'SHUTDOWN';
SIBLINGS:                     S I B L I N G S;
SID:                          'SID';
SIGNAL_COMPONENT:             'SIGNAL_COMPONENT';
SIGNAL_FUNCTION:              'SIGNAL_FUNCTION';
SIGN:                         S I G N;
SIGNTYPE:                     'SIGNTYPE';
SIMPLE_INTEGER:               'SIMPLE_INTEGER';
SIMPLE:                       'SIMPLE';
SINGLE:                       S I N G L E;
SINGLETASK:                   'SINGLETASK';
SINH:                         'SINH';
SIN:                          S I N;
SIZE:                         S I Z E;
SKIP_EXT_OPTIMIZER:           'SKIP_EXT_OPTIMIZER';
SKIP_:                        S K I P;
SKIP_UNQ_UNUSABLE_IDX:        'SKIP_UNQ_UNUSABLE_IDX';
SKIP_UNUSABLE_INDEXES:        'SKIP_UNUSABLE_INDEXES';
SMALLFILE:                    'SMALLFILE';
SMALLINT:                     S M A L L I N T;
SNAPSHOT:                     S N A P S H O T;
SOME:                         'SOME';
SORT:                         'SORT';
SOUNDEX:                      'SOUNDEX';
SOURCE_FILE_DIRECTORY:        'SOURCE_FILE_DIRECTORY';
SOURCE_FILE_NAME_CONVERT:     'SOURCE_FILE_NAME_CONVERT';
SOURCE:                       'SOURCE';
SPACE_KEYWORD:                'SPACE';
SPECIFICATION:                'SPECIFICATION';
SPFILE:                       'SPFILE';
SPLIT:                        'SPLIT';
SPREADSHEET:                  'SPREADSHEET';
SQLDATA:                      'SQLDATA';
SQLERROR:                     'SQLERROR';
SQLLDR:                       'SQLLDR';
SQL:                          'SQL';
SQL_TRACE:                    'SQL_TRACE';
SQL_TRANSLATION_PROFILE:      'SQL_TRANSLATION_PROFILE';
SQRT:                         S Q R T;
STALE:                        'STALE';
STANDALONE:                   'STANDALONE';
STANDARD_HASH:                'STANDARD_HASH';
STANDBY_MAX_DATA_DELAY:       'STANDBY_MAX_DATA_DELAY';
STANDBYS:                     'STANDBYS';
STANDBY:                      'STANDBY';
STAR:                         'STAR';
STAR_TRANSFORMATION:          'STAR_TRANSFORMATION';
START:                        S T A R T;
STARTUP:                      'STARTUP';
STATEMENT_ID:                 S T A T E M E N T '_' I D;
STATEMENT_QUEUING:            'STATEMENT_QUEUING';
STATEMENTS:                   'STATEMENTS';
STATEMENT:                    'STATEMENT';
STATE:                        'STATE';
STATIC:                       'STATIC';
STATISTICS:                   'STATISTICS';
STATS_BINOMIAL_TEST:          'STATS_BINOMIAL_TEST';
STATS_CROSSTAB:               'STATS_CROSSTAB';
STATS_F_TEST:                 'STATS_F_TEST';
STATS_KS_TEST:                'STATS_KS_TEST';
STATS_MODE:                   'STATS_MODE';
STATS_MW_TEST:                'STATS_MW_TEST';
STATS_ONE_WAY_ANOVA:          'STATS_ONE_WAY_ANOVA';
STATS_T_TEST_INDEP:           'STATS_T_TEST_INDEP';
STATS_T_TEST_INDEPU:          'STATS_T_TEST_INDEPU';
STATS_T_TEST_ONE:             'STATS_T_TEST_ONE';
STATS_T_TEST_PAIRED:          'STATS_T_TEST_PAIRED';
STATS_WSR_TEST:               'STATS_WSR_TEST';
STDDEV_POP:                   'STDDEV_POP';
STDDEV_SAMP:                  'STDDEV_SAMP';
STOP:                         'STOP';
STORAGE:                      S T O R A G E;
STORE:                        S T O R E;
STREAMS:                      'STREAMS';
STREAM:                       'STREAM';
STRICT:                       'STRICT';
STRING:                       S T R I N G;
STRIPE_COLUMNS:               'STRIPE_COLUMNS';
STRIPE_WIDTH:                 'STRIPE_WIDTH';
STRIP:                        'STRIP';
STRUCTURE:                    'STRUCTURE';
SUBMULTISET:                  'SUBMULTISET';
SUBPARTITION_REL:             'SUBPARTITION_REL';
SUBPARTITIONS:                'SUBPARTITIONS';
SUBPARTITION:                 'SUBPARTITION';
SUBQUERIES:                   'SUBQUERIES';
SUBQUERY_PRUNING:             'SUBQUERY_PRUNING';
SUBSCRIBE:                    'SUBSCRIBE';
SUBSET:                       'SUBSET';
SUBSTITUTABLE:                'SUBSTITUTABLE';
SUBSTR2:                      'SUBSTR2';
SUBSTR4:                      'SUBSTR4';
SUBSTRB:                      'SUBSTRB';
SUBSTRC:                      'SUBSTRC';
SUBTYPE:                      'SUBTYPE';
SUCCESSFUL:                   'SUCCESSFUL';
SUCCESS:                      'SUCCESS';
SUMMARY:                      'SUMMARY';
SUPPLEMENTAL:                 'SUPPLEMENTAL';
SUSPEND:                      'SUSPEND';
SWAP_JOIN_INPUTS:             'SWAP_JOIN_INPUTS';
SWITCHOVER:                   'SWITCHOVER';
SWITCH:                       'SWITCH';
SYNCHRONOUS:                  'SYNCHRONOUS';
SYNC:                         'SYNC';
SYNONYM:                      S Y N O N Y M;
SYSASM:                       'SYSASM';
SYS_AUDIT:                    'SYS_AUDIT';
SYSAUX:                       'SYSAUX';
SYSBACKUP:                    'SYSBACKUP';
SYS_CHECKACL:                 'SYS_CHECKACL';
SYS_CHECK_PRIVILEGE:          'SYS_CHECK_PRIVILEGE';
SYS_CONNECT_BY_PATH:          'SYS_CONNECT_BY_PATH';
SYS_CONTEXT:                  'SYS_CONTEXT';
SYSDATE:                      (S Y S D A T E) | (N O W);
SYSDBA:                       'SYSDBA';
SYS_DBURIGEN:                 'SYS_DBURIGEN';
SYSDG:                        'SYSDG';
SYS_DL_CURSOR:                'SYS_DL_CURSOR';
SYS_DM_RXFORM_CHR:            'SYS_DM_RXFORM_CHR';
SYS_DM_RXFORM_NUM:            'SYS_DM_RXFORM_NUM';
SYS_DOM_COMPARE:              'SYS_DOM_COMPARE';
SYS_DST_PRIM2SEC:             'SYS_DST_PRIM2SEC';
SYS_DST_SEC2PRIM:             'SYS_DST_SEC2PRIM';
SYS_ET_BFILE_TO_RAW:          'SYS_ET_BFILE_TO_RAW';
SYS_ET_BLOB_TO_IMAGE:         'SYS_ET_BLOB_TO_IMAGE';
SYS_ET_IMAGE_TO_BLOB:         'SYS_ET_IMAGE_TO_BLOB';
SYS_ET_RAW_TO_BFILE:          'SYS_ET_RAW_TO_BFILE';
SYS_EXTPDTXT:                 'SYS_EXTPDTXT';
SYS_EXTRACT_UTC:              'SYS_EXTRACT_UTC';
SYS_FBT_INSDEL:               'SYS_FBT_INSDEL';
SYS_FILTER_ACLS:              'SYS_FILTER_ACLS';
SYS_FNMATCHES:                'SYS_FNMATCHES';
SYS_FNREPLACE:                'SYS_FNREPLACE';
SYS_GET_ACLIDS:               'SYS_GET_ACLIDS';
SYS_GET_COL_ACLIDS:           'SYS_GET_COL_ACLIDS';
SYS_GET_PRIVILEGES:           'SYS_GET_PRIVILEGES';
SYS_GETTOKENID:               'SYS_GETTOKENID';
SYS_GETXTIVAL:                'SYS_GETXTIVAL';
SYS_GUID:                     'SYS_GUID';
SYSGUID:                      'SYSGUID';
SYSKM:                        'SYSKM';
SYS_MAKE_XMLNODEID:           'SYS_MAKE_XMLNODEID';
SYS_MAKEXML:                  'SYS_MAKEXML';
SYS_MKXMLATTR:                'SYS_MKXMLATTR';
SYS_MKXTI:                    'SYS_MKXTI';
SYSOBJ:                       'SYSOBJ';
SYS_OP_ADT2BIN:               'SYS_OP_ADT2BIN';
SYS_OP_ADTCONS:               'SYS_OP_ADTCONS';
SYS_OP_ALSCRVAL:              'SYS_OP_ALSCRVAL';
SYS_OP_ATG:                   'SYS_OP_ATG';
SYS_OP_BIN2ADT:               'SYS_OP_BIN2ADT';
SYS_OP_BITVEC:                'SYS_OP_BITVEC';
SYS_OP_BL2R:                  'SYS_OP_BL2R';
SYS_OP_BLOOM_FILTER_LIST:     'SYS_OP_BLOOM_FILTER_LIST';
SYS_OP_BLOOM_FILTER:          'SYS_OP_BLOOM_FILTER';
SYS_OP_C2C:                   'SYS_OP_C2C';
SYS_OP_CAST:                  'SYS_OP_CAST';
SYS_OP_CEG:                   'SYS_OP_CEG';
SYS_OP_CL2C:                  'SYS_OP_CL2C';
SYS_OP_COMBINED_HASH:         'SYS_OP_COMBINED_HASH';
SYS_OP_COMP:                  'SYS_OP_COMP';
SYS_OP_CONVERT:               'SYS_OP_CONVERT';
SYS_OP_COUNTCHG:              'SYS_OP_COUNTCHG';
SYS_OP_CSCONV:                'SYS_OP_CSCONV';
SYS_OP_CSCONVTEST:            'SYS_OP_CSCONVTEST';
SYS_OP_CSR:                   'SYS_OP_CSR';
SYS_OP_CSX_PATCH:             'SYS_OP_CSX_PATCH';
SYS_OP_CYCLED_SEQ:            'SYS_OP_CYCLED_SEQ';
SYS_OP_DECOMP:                'SYS_OP_DECOMP';
SYS_OP_DESCEND:               'SYS_OP_DESCEND';
SYS_OP_DISTINCT:              'SYS_OP_DISTINCT';
SYS_OP_DRA:                   'SYS_OP_DRA';
SYS_OP_DUMP:                  'SYS_OP_DUMP';
SYS_OP_DV_CHECK:              'SYS_OP_DV_CHECK';
SYS_OP_ENFORCE_NOT_NULL:      'SYS_OP_ENFORCE_NOT_NULL$';
SYSOPER:                      'SYSOPER';
SYS_OP_EXTRACT:               'SYS_OP_EXTRACT';
SYS_OP_GROUPING:              'SYS_OP_GROUPING';
SYS_OP_GUID:                  'SYS_OP_GUID';
SYS_OP_HASH:                  'SYS_OP_HASH';
SYS_OP_IIX:                   'SYS_OP_IIX';
SYS_OP_ITR:                   'SYS_OP_ITR';
SYS_OP_KEY_VECTOR_CREATE:     'SYS_OP_KEY_VECTOR_CREATE';
SYS_OP_KEY_VECTOR_FILTER_LIST: 'SYS_OP_KEY_VECTOR_FILTER_LIST';
SYS_OP_KEY_VECTOR_FILTER:     'SYS_OP_KEY_VECTOR_FILTER';
SYS_OP_KEY_VECTOR_SUCCEEDED:  'SYS_OP_KEY_VECTOR_SUCCEEDED';
SYS_OP_KEY_VECTOR_USE:        'SYS_OP_KEY_VECTOR_USE';
SYS_OP_LBID:                  'SYS_OP_LBID';
SYS_OP_LOBLOC2BLOB:           'SYS_OP_LOBLOC2BLOB';
SYS_OP_LOBLOC2CLOB:           'SYS_OP_LOBLOC2CLOB';
SYS_OP_LOBLOC2ID:             'SYS_OP_LOBLOC2ID';
SYS_OP_LOBLOC2NCLOB:          'SYS_OP_LOBLOC2NCLOB';
SYS_OP_LOBLOC2TYP:            'SYS_OP_LOBLOC2TYP';
SYS_OP_LSVI:                  'SYS_OP_LSVI';
SYS_OP_LVL:                   'SYS_OP_LVL';
SYS_OP_MAKEOID:               'SYS_OP_MAKEOID';
SYS_OP_MAP_NONNULL:           'SYS_OP_MAP_NONNULL';
SYS_OP_MSR:                   'SYS_OP_MSR';
SYS_OP_NICOMBINE:             'SYS_OP_NICOMBINE';
SYS_OP_NIEXTRACT:             'SYS_OP_NIEXTRACT';
SYS_OP_NII:                   'SYS_OP_NII';
SYS_OP_NIX:                   'SYS_OP_NIX';
SYS_OP_NOEXPAND:              'SYS_OP_NOEXPAND';
SYS_OP_NTCIMG:                'SYS_OP_NTCIMG$';
SYS_OP_NUMTORAW:              'SYS_OP_NUMTORAW';
SYS_OP_OIDVALUE:              'SYS_OP_OIDVALUE';
SYS_OP_OPNSIZE:               'SYS_OP_OPNSIZE';
SYS_OP_PAR_1:                 'SYS_OP_PAR_1';
SYS_OP_PARGID_1:              'SYS_OP_PARGID_1';
SYS_OP_PARGID:                'SYS_OP_PARGID';
SYS_OP_PAR:                   'SYS_OP_PAR';
SYS_OP_PART_ID:               'SYS_OP_PART_ID';
SYS_OP_PIVOT:                 'SYS_OP_PIVOT';
SYS_OP_R2O:                   'SYS_OP_R2O';
SYS_OP_RAWTONUM:              'SYS_OP_RAWTONUM';
SYS_OP_RDTM:                  'SYS_OP_RDTM';
SYS_OP_REF:                   'SYS_OP_REF';
SYS_OP_RMTD:                  'SYS_OP_RMTD';
SYS_OP_ROWIDTOOBJ:            'SYS_OP_ROWIDTOOBJ';
SYS_OP_RPB:                   'SYS_OP_RPB';
SYS_OPTLOBPRBSC:              'SYS_OPTLOBPRBSC';
SYS_OP_TOSETID:               'SYS_OP_TOSETID';
SYS_OP_TPR:                   'SYS_OP_TPR';
SYS_OP_TRTB:                  'SYS_OP_TRTB';
SYS_OPTXICMP:                 'SYS_OPTXICMP';
SYS_OPTXQCASTASNQ:            'SYS_OPTXQCASTASNQ';
SYS_OP_UNDESCEND:             'SYS_OP_UNDESCEND';
SYS_OP_VECAND:                'SYS_OP_VECAND';
SYS_OP_VECBIT:                'SYS_OP_VECBIT';
SYS_OP_VECOR:                 'SYS_OP_VECOR';
SYS_OP_VECXOR:                'SYS_OP_VECXOR';
SYS_OP_VERSION:               'SYS_OP_VERSION';
SYS_OP_VREF:                  'SYS_OP_VREF';
SYS_OP_VVD:                   'SYS_OP_VVD';
SYS_OP_XMLCONS_FOR_CSX:       'SYS_OP_XMLCONS_FOR_CSX';
SYS_OP_XPTHATG:               'SYS_OP_XPTHATG';
SYS_OP_XPTHIDX:               'SYS_OP_XPTHIDX';
SYS_OP_XPTHOP:                'SYS_OP_XPTHOP';
SYS_OP_XTXT2SQLT:             'SYS_OP_XTXT2SQLT';
SYS_OP_ZONE_ID:               'SYS_OP_ZONE_ID';
SYS_ORDERKEY_DEPTH:           'SYS_ORDERKEY_DEPTH';
SYS_ORDERKEY_MAXCHILD:        'SYS_ORDERKEY_MAXCHILD';
SYS_ORDERKEY_PARENT:          'SYS_ORDERKEY_PARENT';
SYS_PARALLEL_TXN:             'SYS_PARALLEL_TXN';
SYS_PATHID_IS_ATTR:           'SYS_PATHID_IS_ATTR';
SYS_PATHID_IS_NMSPC:          'SYS_PATHID_IS_NMSPC';
SYS_PATHID_LASTNAME:          'SYS_PATHID_LASTNAME';
SYS_PATHID_LASTNMSPC:         'SYS_PATHID_LASTNMSPC';
SYS_PATH_REVERSE:             'SYS_PATH_REVERSE';
SYS_PXQEXTRACT:               'SYS_PXQEXTRACT';
SYS_RAW_TO_XSID:              'SYS_RAW_TO_XSID';
SYS_RID_ORDER:                'SYS_RID_ORDER';
SYS_ROW_DELTA:                'SYS_ROW_DELTA';
SYS_SC_2_XMLT:                'SYS_SC_2_XMLT';
SYS_SYNRCIREDO:               'SYS_SYNRCIREDO';
SYSTEM_DEFINED:               'SYSTEM_DEFINED';
SYSTEM:                       'SYSTEM';
SYSTIMESTAMP:                 'SYSTIMESTAMP';
SYS_TYPEID:                   'SYS_TYPEID';
SYS_UMAKEXML:                 'SYS_UMAKEXML';
SYS_XMLANALYZE:               'SYS_XMLANALYZE';
SYS_XMLCONTAINS:              'SYS_XMLCONTAINS';
SYS_XMLCONV:                  'SYS_XMLCONV';
SYS_XMLEXNSURI:               'SYS_XMLEXNSURI';
SYS_XMLGEN:                   'SYS_XMLGEN';
SYS_XMLI_LOC_ISNODE:          'SYS_XMLI_LOC_ISNODE';
SYS_XMLI_LOC_ISTEXT:          'SYS_XMLI_LOC_ISTEXT';
SYS_XMLINSTR:                 'SYS_XMLINSTR';
SYS_XMLLOCATOR_GETSVAL:       'SYS_XMLLOCATOR_GETSVAL';
SYS_XMLNODEID_GETCID:         'SYS_XMLNODEID_GETCID';
SYS_XMLNODEID_GETLOCATOR:     'SYS_XMLNODEID_GETLOCATOR';
SYS_XMLNODEID_GETOKEY:        'SYS_XMLNODEID_GETOKEY';
SYS_XMLNODEID_GETPATHID:      'SYS_XMLNODEID_GETPATHID';
SYS_XMLNODEID_GETPTRID:       'SYS_XMLNODEID_GETPTRID';
SYS_XMLNODEID_GETRID:         'SYS_XMLNODEID_GETRID';
SYS_XMLNODEID_GETSVAL:        'SYS_XMLNODEID_GETSVAL';
SYS_XMLNODEID_GETTID:         'SYS_XMLNODEID_GETTID';
SYS_XMLNODEID:                'SYS_XMLNODEID';
SYS_XMLT_2_SC:                'SYS_XMLT_2_SC';
SYS_XMLTRANSLATE:             'SYS_XMLTRANSLATE';
SYS_XMLTYPE2SQL:              'SYS_XMLTYPE2SQL';
SYS_XQ_ASQLCNV:               'SYS_XQ_ASQLCNV';
SYS_XQ_ATOMCNVCHK:            'SYS_XQ_ATOMCNVCHK';
SYS_XQBASEURI:                'SYS_XQBASEURI';
SYS_XQCASTABLEERRH:           'SYS_XQCASTABLEERRH';
SYS_XQCODEP2STR:              'SYS_XQCODEP2STR';
SYS_XQCODEPEQ:                'SYS_XQCODEPEQ';
SYS_XQCON2SEQ:                'SYS_XQCON2SEQ';
SYS_XQCONCAT:                 'SYS_XQCONCAT';
SYS_XQDELETE:                 'SYS_XQDELETE';
SYS_XQDFLTCOLATION:           'SYS_XQDFLTCOLATION';
SYS_XQDOC:                    'SYS_XQDOC';
SYS_XQDOCURI:                 'SYS_XQDOCURI';
SYS_XQDURDIV:                 'SYS_XQDURDIV';
SYS_XQED4URI:                 'SYS_XQED4URI';
SYS_XQENDSWITH:               'SYS_XQENDSWITH';
SYS_XQERRH:                   'SYS_XQERRH';
SYS_XQERR:                    'SYS_XQERR';
SYS_XQESHTMLURI:              'SYS_XQESHTMLURI';
SYS_XQEXLOBVAL:               'SYS_XQEXLOBVAL';
SYS_XQEXSTWRP:                'SYS_XQEXSTWRP';
SYS_XQEXTRACT:                'SYS_XQEXTRACT';
SYS_XQEXTRREF:                'SYS_XQEXTRREF';
SYS_XQEXVAL:                  'SYS_XQEXVAL';
SYS_XQFB2STR:                 'SYS_XQFB2STR';
SYS_XQFNBOOL:                 'SYS_XQFNBOOL';
SYS_XQFNCMP:                  'SYS_XQFNCMP';
SYS_XQFNDATIM:                'SYS_XQFNDATIM';
SYS_XQFNLNAME:                'SYS_XQFNLNAME';
SYS_XQFNNM:                   'SYS_XQFNNM';
SYS_XQFNNSURI:                'SYS_XQFNNSURI';
SYS_XQFNPREDTRUTH:            'SYS_XQFNPREDTRUTH';
SYS_XQFNQNM:                  'SYS_XQFNQNM';
SYS_XQFNROOT:                 'SYS_XQFNROOT';
SYS_XQFORMATNUM:              'SYS_XQFORMATNUM';
SYS_XQFTCONTAIN:              'SYS_XQFTCONTAIN';
SYS_XQFUNCR:                  'SYS_XQFUNCR';
SYS_XQGETCONTENT:             'SYS_XQGETCONTENT';
SYS_XQINDXOF:                 'SYS_XQINDXOF';
SYS_XQINSERT:                 'SYS_XQINSERT';
SYS_XQINSPFX:                 'SYS_XQINSPFX';
SYS_XQIRI2URI:                'SYS_XQIRI2URI';
SYS_XQLANG:                   'SYS_XQLANG';
SYS_XQLLNMFRMQNM:             'SYS_XQLLNMFRMQNM';
SYS_XQMKNODEREF:              'SYS_XQMKNODEREF';
SYS_XQNILLED:                 'SYS_XQNILLED';
SYS_XQNODENAME:               'SYS_XQNODENAME';
SYS_XQNORMSPACE:              'SYS_XQNORMSPACE';
SYS_XQNORMUCODE:              'SYS_XQNORMUCODE';
SYS_XQ_NRNG:                  'SYS_XQ_NRNG';
SYS_XQNSP4PFX:                'SYS_XQNSP4PFX';
SYS_XQNSPFRMQNM:              'SYS_XQNSPFRMQNM';
SYS_XQPFXFRMQNM:              'SYS_XQPFXFRMQNM';
SYS_XQ_PKSQL2XML:             'SYS_XQ_PKSQL2XML';
SYS_XQPOLYABS:                'SYS_XQPOLYABS';
SYS_XQPOLYADD:                'SYS_XQPOLYADD';
SYS_XQPOLYCEL:                'SYS_XQPOLYCEL';
SYS_XQPOLYCSTBL:              'SYS_XQPOLYCSTBL';
SYS_XQPOLYCST:                'SYS_XQPOLYCST';
SYS_XQPOLYDIV:                'SYS_XQPOLYDIV';
SYS_XQPOLYFLR:                'SYS_XQPOLYFLR';
SYS_XQPOLYMOD:                'SYS_XQPOLYMOD';
SYS_XQPOLYMUL:                'SYS_XQPOLYMUL';
SYS_XQPOLYRND:                'SYS_XQPOLYRND';
SYS_XQPOLYSQRT:               'SYS_XQPOLYSQRT';
SYS_XQPOLYSUB:                'SYS_XQPOLYSUB';
SYS_XQPOLYUMUS:               'SYS_XQPOLYUMUS';
SYS_XQPOLYUPLS:               'SYS_XQPOLYUPLS';
SYS_XQPOLYVEQ:                'SYS_XQPOLYVEQ';
SYS_XQPOLYVGE:                'SYS_XQPOLYVGE';
SYS_XQPOLYVGT:                'SYS_XQPOLYVGT';
SYS_XQPOLYVLE:                'SYS_XQPOLYVLE';
SYS_XQPOLYVLT:                'SYS_XQPOLYVLT';
SYS_XQPOLYVNE:                'SYS_XQPOLYVNE';
SYS_XQREF2VAL:                'SYS_XQREF2VAL';
SYS_XQRENAME:                 'SYS_XQRENAME';
SYS_XQREPLACE:                'SYS_XQREPLACE';
SYS_XQRESVURI:                'SYS_XQRESVURI';
SYS_XQRNDHALF2EVN:            'SYS_XQRNDHALF2EVN';
SYS_XQRSLVQNM:                'SYS_XQRSLVQNM';
SYS_XQRYENVPGET:              'SYS_XQRYENVPGET';
SYS_XQRYVARGET:               'SYS_XQRYVARGET';
SYS_XQRYWRP:                  'SYS_XQRYWRP';
SYS_XQSEQ2CON4XC:             'SYS_XQSEQ2CON4XC';
SYS_XQSEQ2CON:                'SYS_XQSEQ2CON';
SYS_XQSEQDEEPEQ:              'SYS_XQSEQDEEPEQ';
SYS_XQSEQINSB:                'SYS_XQSEQINSB';
SYS_XQSEQRM:                  'SYS_XQSEQRM';
SYS_XQSEQRVS:                 'SYS_XQSEQRVS';
SYS_XQSEQSUB:                 'SYS_XQSEQSUB';
SYS_XQSEQTYPMATCH:            'SYS_XQSEQTYPMATCH';
SYS_XQSTARTSWITH:             'SYS_XQSTARTSWITH';
SYS_XQSTATBURI:               'SYS_XQSTATBURI';
SYS_XQSTR2CODEP:              'SYS_XQSTR2CODEP';
SYS_XQSTRJOIN:                'SYS_XQSTRJOIN';
SYS_XQSUBSTRAFT:              'SYS_XQSUBSTRAFT';
SYS_XQSUBSTRBEF:              'SYS_XQSUBSTRBEF';
SYS_XQTOKENIZE:               'SYS_XQTOKENIZE';
SYS_XQTREATAS:                'SYS_XQTREATAS';
SYS_XQ_UPKXML2SQL:            'SYS_XQ_UPKXML2SQL';
SYS_XQXFORM:                  'SYS_XQXFORM';
SYS_XSID_TO_RAW:              'SYS_XSID_TO_RAW';
SYS_ZMAP_FILTER:              'SYS_ZMAP_FILTER';
SYS_ZMAP_REFRESH:             'SYS_ZMAP_REFRESH';
TABLE_LOOKUP_BY_NL:           'TABLE_LOOKUP_BY_NL';
TABLESPACE_NO:                'TABLESPACE_NO';
TABLESPACE:                   T A B L E S P A C E;
TABLES:                       'TABLES';
TABLE_STATS:                  'TABLE_STATS';
TABLE:                        T A B L E;
TABNO:                        'TABNO';
TAG:                          'TAG';
TANH:                         'TANH';
TAN:                          T A N;
TBLORIDXPARTNUM:              'TBL$OR$IDX$PART$NUM';
TEMPFILE:                     'TEMPFILE';
TEMPLATE:                     'TEMPLATE';
TEMPORARY:                    'TEMPORARY';
TEMP_TABLE:                   'TEMP_TABLE';
TEST_:                        'TEST'; // conflicts with GTEST macros
TEXT:                         T E X T;
THAN:                         'THAN';
THEN:                         T H E N;
THE:                          'THE';
THREAD:                       'THREAD';
THROUGH:                      'THROUGH';
TIER:                         'TIER';
TIES:                         T I E S;
TIMEOUT:                      'TIMEOUT';
TIMESTAMP_LTZ_UNCONSTRAINED:  'TIMESTAMP_LTZ_UNCONSTRAINED';
TIMESTAMP:                    T I M E S T A M P;
TIMESTAMP_TZ_UNCONSTRAINED:   'TIMESTAMP_TZ_UNCONSTRAINED';
TIMESTAMP_UNCONSTRAINED:      'TIMESTAMP_UNCONSTRAINED';
TIMES:                        T I M E S;
TIME:                         'TIME';
TIMEZONE:                     'TIMEZONE';
TIMEZONE_ABBR:                'TIMEZONE_ABBR';
TIMEZONE_HOUR:                'TIMEZONE_HOUR';
TIMEZONE_MINUTE:              'TIMEZONE_MINUTE';
TIMEZONE_OFFSET:              'TIMEZONE_OFFSET';
TIMEZONE_REGION:              'TIMEZONE_REGION';
TIME_ZONE:                    'TIME_ZONE';
TIV_GB:                       'TIV_GB';
TIV_SSF:                      'TIV_SSF';
TO_ACLID:                     T O '_' A C L I D;
TO_BINARY_DOUBLE:             T O '_' B I N A R Y '_' D O U B L E;
TO_BINARY_FLOAT:              T O '_' B I N A R Y '_' F L O A T;
TO_BLOB:                      T O '_' B L O B;
TO_CLOB:                      T O '_' C L O B;
TO_DSINTERVAL:                T O '_' D S I N T E R V A L;
TO_LOB:                       T O '_' L O B;
TO_MULTI_BYTE:                T O '_' M U L T I '_' B Y T E;
TO_NCHAR:                     T O '_' N C H A R;
TO_NCLOB:                     T O '_' N C L O B;
TO_NUMBER:                    T O '_'? N U M B E R;
TOPLEVEL:                     T O P L E V E L;
TO_SINGLE_BYTE:               T O '_' S I N G L E '_' B Y T E;
TO_TIMESTAMP:                 T O '_' T I M E S T A M P;
TO_TIMESTAMP_TZ:              T O '_' T I M E S T A M P '_' T Z;
TO_TIME:                      T O '_' T I M E;
TO_TIME_TZ:                   T O '_' T I M E '_' T Z;
TO:                           T O;
TO_YMINTERVAL:                T O '_' Y M I N T E R V A L;
TRACE:                        'TRACE';
TRACING:                      'TRACING';
TRACKING:                     'TRACKING';
TRAILING:                     T R A I L I N G;
TRANSACTION:                  'TRANSACTION';
TRANSFORM_DISTINCT_AGG:       'TRANSFORM_DISTINCT_AGG';
TRANSITIONAL:                 'TRANSITIONAL';
TRANSITION:                   'TRANSITION';
TRANSLATE:                    'TRANSLATE';
TRANSLATION:                  'TRANSLATION';
TREAT:                        'TREAT';
TRIGGERS:                     'TRIGGERS';
TRIGGER:                      'TRIGGER';
TRUE:                         T R U E;
TRUNCATE:                     'TRUNCATE';
TRUNC:                        'TRUNC';
TRUSTED:                      'TRUSTED';
TRUST:                        'TRUST';
TUNING:                       'TUNING';
TX:                           'TX';
TYPES:                        'TYPES';
TYPE:                         T Y P E;
TZ_OFFSET:                    'TZ_OFFSET';
UB2:                          'UB2';
UBA:                          'UBA';
UCS2:                         'UCS2';
UID:                          'UID';
UNARCHIVED:                   'UNARCHIVED';
UNBOUNDED:                    'UNBOUNDED';
UNBOUND:                      'UNBOUND';
UNCONDITIONAL:                'UNCONDITIONAL';
UNDER:                        'UNDER';
UNDO:                         'UNDO';
UNDROP:                       'UNDROP';
UNIFORM:                      'UNIFORM';
UNION:                        U N I O N;
UNIQUE:                       U N I Q U E;
UNISTR:                       'UNISTR';
UNLIMITED:                    'UNLIMITED';
UNLOAD:                       'UNLOAD';
UNLOCK:                       'UNLOCK';
UNMATCHED:                    'UNMATCHED';
UNNEST_INNERJ_DISTINCT_VIEW:  'UNNEST_INNERJ_DISTINCT_VIEW';
UNNEST_NOSEMIJ_NODISTINCTVIEW: 'UNNEST_NOSEMIJ_NODISTINCTVIEW';
UNNEST_SEMIJ_VIEW:            'UNNEST_SEMIJ_VIEW';
UNNEST:                       'UNNEST';
UNPACKED:                     'UNPACKED';
UNPIVOT:                      U N P I V O T;
UNPLUG:                       'UNPLUG';
UNPROTECTED:                  'UNPROTECTED';
UNQUIESCE:                    'UNQUIESCE';
UNRECOVERABLE:                'UNRECOVERABLE';
UNRESTRICTED:                 'UNRESTRICTED';
UNSUBSCRIBE:                  'UNSUBSCRIBE';
UNTIL:                        'UNTIL';
UNUSABLE:                     'UNUSABLE';
UNUSED:                       'UNUSED';
UP:                           U P;
UPDATABLE:                    'UPDATABLE';
UPDATED:                      'UPDATED';
UPDATE:                       U P D A T E;
UPDATEXML:                    'UPDATEXML';
UPD_INDEXES:                  'UPD_INDEXES';
UPD_JOININDEX:                'UPD_JOININDEX';
UPGRADE:                      'UPGRADE';
UPPER:                        U P P E R;
UPSERT:                       U P S E R T;
UROWID:                       'UROWID';
USABLE:                       'USABLE';
USAGE:                        'USAGE';
USE_ANTI:                     'USE_ANTI';
USE_CONCAT:                   'USE_CONCAT';
USE_CUBE:                     'USE_CUBE';
USE_HASH_AGGREGATION:         'USE_HASH_AGGREGATION';
USE_HASH_GBY_FOR_PUSHDOWN:    'USE_HASH_GBY_FOR_PUSHDOWN';
USE_HASH:                     'USE_HASH';
USE_HIDDEN_PARTITIONS:        'USE_HIDDEN_PARTITIONS';
USE_INVISIBLE_INDEXES:        'USE_INVISIBLE_INDEXES';
USE_MERGE_CARTESIAN:          'USE_MERGE_CARTESIAN';
USE_MERGE:                    'USE_MERGE';
USE_NL:                       'USE_NL';
USE_NL_WITH_INDEX:            'USE_NL_WITH_INDEX';
USE_PRIVATE_OUTLINES:         'USE_PRIVATE_OUTLINES';
USER_DATA:                    'USER_DATA';
USER_DEFINED:                 'USER_DEFINED';
USERENV:                      'USERENV';
USERGROUP:                    'USERGROUP';
USER_RECYCLEBIN:              'USER_RECYCLEBIN';
USERS:                        U S E R S;
USER_TABLESPACES:             'USER_TABLESPACES';
USER:                         'USER';
USE_SEMI:                     'USE_SEMI';
USE_STORED_OUTLINES:          'USE_STORED_OUTLINES';
USE_TTT_FOR_GSETS:            'USE_TTT_FOR_GSETS';
USE:                          'USE';
USE_VECTOR_AGGREGATION:       'USE_VECTOR_AGGREGATION';
USE_WEAK_NAME_RESL:           'USE_WEAK_NAME_RESL';
USING_NO_EXPAND:              'USING_NO_EXPAND';
USING:                        U S I N G;
UTF16BE:                      'UTF16BE';
UTF16LE:                      'UTF16LE';
UTF32:                        'UTF32';
UTF8:                         'UTF8';
UUID:                         U U I D;
V1:                           'V1';
V2:                           'V2';
VALIDATE:                     'VALIDATE';
VALIDATION:                   'VALIDATION';
VALID_TIME_END:               'VALID_TIME_END';
VALUES:                       V A L U E S;
VALUE:                        V A L U E;
VARCHAR2:                     V A R C H A R '2';
VARCHAR:                      V A R C H A R;
VARIABLE:                     'VARIABLE';
VAR_POP:                      'VAR_POP';
VARRAYS:                      'VARRAYS';
VARRAY:                       'VARRAY';
VAR_SAMP:                     'VAR_SAMP';
VARYING:                      'VARYING';
VECTOR_READ_TRACE:            'VECTOR_READ_TRACE';
VECTOR_READ:                  'VECTOR_READ';
VECTOR_TRANSFORM_DIMS:        'VECTOR_TRANSFORM_DIMS';
VECTOR_TRANSFORM_FACT:        'VECTOR_TRANSFORM_FACT';
VECTOR_TRANSFORM:             'VECTOR_TRANSFORM';
VERIFIER:                     'VERIFIER';
VERIFY:                       'VERIFY';
VERSIONING:                   'VERSIONING';
VERSIONS_ENDSCN:              'VERSIONS_ENDSCN';
VERSIONS_ENDTIME:             'VERSIONS_ENDTIME';
VERSIONS_OPERATION:           'VERSIONS_OPERATION';
VERSIONS_STARTSCN:            'VERSIONS_STARTSCN';
VERSIONS_STARTTIME:           'VERSIONS_STARTTIME';
VERSIONS:                     'VERSIONS';
VERSIONS_XID:                 'VERSIONS_XID';
VERSION:                      'VERSION';
VIEW:                         'VIEW';
VIOLATION:                    'VIOLATION';
VIRTUAL:                      'VIRTUAL';
VISIBILITY:                   'VISIBILITY';
VISIBLE:                      'VISIBLE';
VOLUME:                       'VOLUME';
VSIZE:                        'VSIZE';
WAIT:                         W A I T;
WALLET:                       'WALLET';
WARNING:                      'WARNING';
WEEKS:                        W E E K S;
WEEK:                         W E E K;
WELLFORMED:                   'WELLFORMED';
WHENEVER:                     'WHENEVER';
WHEN:                         W H E N;
WHERE:                        W H E R E;
WHILE:                        'WHILE';
WHITESPACE:                   'WHITESPACE';
WIDTH_BUCKET:                 'WIDTH_BUCKET';
WITHIN:                       W I T H I N;
WITHOUT:                      'WITHOUT';
WITH_PLSQL:                   'WITH_PLSQL';
WITH:                         W I T H;
WORK:                         'WORK';
WRAPPED:                      'WRAPPED';
WRAPPER:                      'WRAPPER';
WRITE:                        'WRITE';
XDB_FASTPATH_INSERT:          'XDB_FASTPATH_INSERT';
XDB:                          'XDB';
X_DYN_PRUNE:                  'X_DYN_PRUNE';
XID:                          'XID';
XML2OBJECT:                   'XML2OBJECT';
XMLAGG:                       'XMLAGG';
XMLATTRIBUTES:                'XMLATTRIBUTES';
XMLCAST:                      X M L C A S T;
XMLCDATA:                     'XMLCDATA';
XMLCOLATTVAL:                 'XMLCOLATTVAL';
XMLCOMMENT:                   'XMLCOMMENT';
XMLCONCAT:                    'XMLCONCAT';
XMLDIFF:                      'XMLDIFF';
XML_DML_RWT_STMT:             'XML_DML_RWT_STMT';
XMLELEMENT:                   'XMLELEMENT';
XMLEXISTS2:                   'XMLEXISTS2';
XMLEXISTS:                    'XMLEXISTS';
XMLFOREST:                    'XMLFOREST';
XMLINDEX:                     'XMLINDEX';
XMLINDEX_REWRITE_IN_SELECT:   'XMLINDEX_REWRITE_IN_SELECT';
XMLINDEX_REWRITE:             'XMLINDEX_REWRITE';
XMLINDEX_SEL_IDX_TBL:         'XMLINDEX_SEL_IDX_TBL';
XMLISNODE:                    'XMLISNODE';
XMLISVALID:                   'XMLISVALID';
XMLNAMESPACES:                'XMLNAMESPACES';
XMLPARSE:                     'XMLPARSE';
XMLPATCH:                     'XMLPATCH';
XMLPI:                        'XMLPI';
XMLQUERYVAL:                  'XMLQUERYVAL';
XMLQUERY:                     X M L Q U E R Y;
XMLROOT:                      'XMLROOT';
XMLSCHEMA:                    X M L S C H E M A;
XMLSERIALIZE:                 'XMLSERIALIZE';
XMLTABLE:                     'XMLTABLE';
XMLTRANSFORMBLOB:             'XMLTRANSFORMBLOB';
XMLTRANSFORM:                 'XMLTRANSFORM';
XMLTYPE:                      X M L T Y P E;
XML:                          X M L;
XPATHTABLE:                   'XPATHTABLE';
XS_SYS_CONTEXT:               'XS_SYS_CONTEXT';
XS:                           'XS';
YEARS:                        Y E A R S;
YEAR:                         Y E A R;
YES:                          'YES';
YMINTERVAL_UNCONSTRAINED:     'YMINTERVAL_UNCONSTRAINED';
ZONEMAP:                      'ZONEMAP';
ZONE:                         'ZONE';
PREDICTION:                   'PREDICTION';
PREDICTION_BOUNDS:            'PREDICTION_BOUNDS';
PREDICTION_COST:              'PREDICTION_COST';
PREDICTION_DETAILS:           'PREDICTION_DETAILS';
PREDICTION_PROBABILITY:       'PREDICTION_PROBABILITY';
PREDICTION_SET:               'PREDICTION_SET';
CUME_DIST:                    C U M E '_' D I S T;
DENSE_RANK:                   D E N S E '_' R A N K;
LISTAGG:                      L I S T A G G;
PERCENT_RANK:                 'PERCENT_RANK';
PERCENTILE:                   P E R C E N T I L E;
PERCENTILE_CONT:              P E R C E N T I L E '_' C O N T;
PERCENTILE_DISC:              P E R C E N T I L E '_' D I S C;
RANK:                         R A N K;
AVG:                          A V G;
CORR:                         'CORR';
COVAR_:                       'COVAR';
DECODE:                       D E C O D E;
LAG:                          'LAG';
LEAD:                         L E A D;
MAX:                          M A X;
MEDIAN:                       M E D I A N;
MIN:                          M I N;
NTILE:                        'NTILE';
NVL:                          N V L;
RATIO_TO_REPORT:              'RATIO_TO_REPORT';
REGR_:                        'REGR';
ROUND:                        R O U N D;
ROW_NUMBER:                   R O W '_' N U M B E R;
SUBSTR:                       S U B S T R (I N G)?;
TO_CHAR:                      T O '_'? C H A R;
TRIM:                         T R I M;
SUM:                          S U M;
STDDEV:                       S T D D E V | S T D E V;
VAR_:                         'VAR';
VARIANCE:                     V A R I A N C E;
LEAST:                        L E A S T;
GREATEST:                     G R E A T E S T;
TO_DATE:                      T O '_'? D A T E;

// Rule #358 <NATIONAL_CHAR_STRING_LIT> - subtoken typecast in <REGULAR_ID>, it also incorporates <character_representation>
//  Lowercase 'n' is a usual addition to the standard

NATIONAL_CHAR_STRING_LIT: 'N' '\'' (~('\'' | '\r' | '\n' ) | '\'' '\'' | NEWLINE)* '\'';

//  Rule #040 <BIT_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'b' is a usual addition to the standard

BIT_STRING_LIT: 'B' ('\'' [01]* '\'')+;

//  Rule #284 <HEX_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'x' is a usual addition to the standard

HEX_STRING_LIT: 'X' ('\'' [A-F0-9]* '\'')+;
DOUBLE_PERIOD:  '..';
PERIOD:         '.';

//{ Rule #238 <EXACT_NUM_LIT>
//  This rule is a bit tricky - it resolves the ambiguity with <PERIOD>
//  It also incorporates <mantisa> and <exponent> for the <APPROXIMATE_NUM_LIT>
//  Rule #501 <signed_integer> was incorporated directly in the token <APPROXIMATE_NUM_LIT>
//  See also the rule #617 <unsigned_num_lit>
/*
    : (
            UNSIGNED_INTEGER
            ( '.' UNSIGNED_INTEGER
            | {$type = UNSIGNED_INTEGER;}
            ) ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    | '.' UNSIGNED_INTEGER ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    )
    (D | F)?
    ;*/

UNSIGNED_INTEGER:    [0-9]+;
APPROXIMATE_NUM_LIT: FLOAT_FRAGMENT (E ('+'|'-')? (FLOAT_FRAGMENT | [0-9]+))? (D | F)?;

// Rule #--- <CHAR_STRING> is a base for Rule #065 <char_string_lit> , it incorporates <character_representation>
// and a superfluous subtoken typecasting of the "QUOTE"
CHAR_STRING: '\'' (~('\'' | '\r' | '\n') | '\'' '\'' | NEWLINE)* '\'';

// Perl-style quoted string, see SQL reference, chapter String Literals
CHAR_STRING_PERL    : 'Q' ( QS_ANGLE | QS_BRACE | QS_BRACK | QS_PAREN) -> type(CHAR_STRING);
fragment QUOTE      : '\'' ;
fragment QS_ANGLE   : QUOTE '<' .*? '>' QUOTE ;
fragment QS_BRACE   : QUOTE '{' .*? '}' QUOTE ;
fragment QS_BRACK   : QUOTE '[' .*? ']' QUOTE ;
fragment QS_PAREN   : QUOTE '(' .*? ')' QUOTE ;
fragment QS_OTHER_CH: ~('<' | '{' | '[' | '(' | ' ' | '\t' | '\n' | '\r');

DELIMITED_ID: '"' (~('"' | '\r' | '\n') | '"' '"')+ '"' ;

// SQL_SPECIAL_CHAR was split into single rules

PERCENT:                   '%';
AMPERSAND:                 '&';
LEFT_PAREN:                '(';
RIGHT_PAREN:               ')';
DOUBLE_ASTERISK:           '**';
ASTERISK:                  '*';
PLUS_SIGN:                 '+';
MINUS_SIGN:                '-';
COMMA:                     ',';
SOLIDUS:                   '/';
AT_SIGN:                   '@';
ASSIGN_OP:                 ':=';

// See OCI reference for more information about this

BINDVAR
    : ':' SIMPLE_LETTER  (SIMPLE_LETTER | [0-9] | '_')*
    | ':' DELIMITED_ID  // not used in SQL but spotted in v$sqltext when using cursor_sharing
    | ':' UNSIGNED_INTEGER
    | QUESTION_MARK // not in SQL, not in Oracle, not in OCI, use this for JDBC
    ;

NOT_EQUAL_OP:              '!='
            |              '<>'
            |              '^='
            |              '~='
            ;
CARRET_OPERATOR_PART:      '^';
TILDE_OPERATOR_PART:       '~';
EXCLAMATION_OPERATOR_PART: '!';
GREATER_THAN_OP:           '>';
LESS_THAN_OP:              '<';
COLON:                     ':';
DOUBLE_COLON:              '::';
SEMICOLON:                 ';';

fragment
QUESTION_MARK: '?';

// protected UNDERSCORE : '_' SEPARATOR ; // subtoken typecast within <INTRODUCER>
BAR: '|';
EQUALS_OP: '=';

// Rule #532 <SQL_EMBDD_LANGUAGE_CHAR> was split into single rules:
LEFT_BRACKET: '[';
RIGHT_BRACKET: ']';

LEFT_CURLY_BRACKET: '{';
RIGHT_CURLY_BRACKET: '}';

//{ Rule #319 <INTRODUCER>
INTRODUCER
    : '_' //(SEPARATOR {$type = UNDERSCORE;})?
    ;

//{ Rule #479 <SEPARATOR>
//  It was originally a protected rule set to be filtered out but the <COMMENT> and <'-'> clashed.
/*SEPARATOR
    : '-' -> type('-')
    | COMMENT -> channel(HIDDEN)
    | (SPACE | NEWLINE)+ -> channel(HIDDEN)
    ;*/
//}

SPACES: [ \t\r\n]+ -> skip;

// Rule #504 <SIMPLE_LETTER> - simple_latin _letter was generalised into SIMPLE_LETTER
//  Unicode is yet to be implemented - see NSF0
fragment
SIMPLE_LETTER
    : [A-Za-z]
    ;

fragment
FLOAT_FRAGMENT
    : UNSIGNED_INTEGER* '.'? UNSIGNED_INTEGER+
    ;

// Rule #097 <COMMENT>

SINGLE_LINE_COMMENT: '--' ~('\r' | '\n')* (NEWLINE | EOF)   -> channel(HIDDEN);
MULTI_LINE_COMMENT:  '/*' .*? '*/'                          -> channel(HIDDEN);

// SQL*Plus prompt
// TODO should be grammar rule, but tricky to implement

PROMPT
    : 'prompt' SPACE ( ~('\r' | '\n') )* (NEWLINE|EOF)
    ;

START_CMD
    // TODO When using full word START there is a conflict with START WITH in sequences and CONNECT BY queries
    // 'start' SPACE ( ~( '\r' | '\n') )* (NEWLINE|EOF)
    : 'sta' SPACE ( ~('\r' | '\n') )* (NEWLINE|EOF)
    // TODO Single @ conflicts with a database link name, like employees@remote
    // | '@' ( ~('\r' | '\n') )* (NEWLINE|EOF)
    | '@@' ( ~('\r' | '\n') )* (NEWLINE|EOF)
    ;

fragment
NEWLINE: '\r'? '\n';

fragment
SPACE: [ \t];

//{ Rule #442 <REGULAR_ID> additionally encapsulates a few STRING_LITs.
//  Within testLiterals all reserved and non-reserved words are being resolved

REGULAR_ID: (SIMPLE_LETTER | CHINESE_CHARACTER) ((SIMPLE_LETTER | CHINESE_CHARACTER) | '$' | '_' | '#' | [0-9])*;

DATE_TIME: '#' (~('\'' | '\r' | '\n' | '#' | ',') | '\'' '\'' | NEWLINE)* '#';

ZV: '@!' -> channel(HIDDEN);
