# from airflow import DAG
# from airflow.hooks.postgres_hook import PostgresHook

# src = PostgresHook(postgres_conn_id='source', schema='test_data_src')
# dest = PostgresHook(postgres_conn_id='dest', schema='test_data_dest')

# src_conn = src.get_conn()
# cursor = src_conn.cursor()
# cursor.execute("SELECT * FROM test_data_src.homes;")
# dest.insert_rows(table='test_data_dest.hoomes', rows=cursor)

# -*- coding: utf-8 -*-
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import print_function
import airflow
from datetime import datetime, timedelta
from acme.operators.dwh_operators import PostgresToPostgresOperator
from acme.operators.dwh_operators import AuditOperator
from airflow.models import Variable


args = {
    'owner': 'airflow',
    'start_date': airflow.utils.dates.days_ago(7),
    'provide_context': True
}

tmpl_search_path = Variable.get("sql_path")

dag = airflow.DAG(
    'sync_homes',
    schedule_interval="@daily",
    dagrun_timeout=timedelta(minutes=60),
    template_searchpath=tmpl_search_path,
    default_args=args,
    max_active_runs=1)

get_auditid = AuditOperator(
    task_id='get_audit_id',
    postgres_conn_id='source',
    audit_key="customer",
    cycle_dtm="{{ ts }}",
    dag=dag,
    pool='source')

extract_homes = PostgresToPostgresOperator(
    sql='SELECT * from test_data_src.homes',
    pg_table='test_data_dest.homes',
    src_postgres_conn_id='source',
    dest_postgress_conn_id='dest',
    pg_preoperator="DELETE FROM test_data_dest.homes",
    parameters={"audit_id": "{{ ti.xcom_pull(task_ids='get_audit_id', key='audit_id') }}"},
    task_id='extract_customer',
    dag=dag,
    pool='postgres_dwh')

get_auditid >> extract_homes


if __name__ == "__main__":
    dag.cli()
