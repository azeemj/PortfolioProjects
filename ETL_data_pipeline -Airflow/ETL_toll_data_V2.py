from datetime import timedelta
# The DAG object; we'll need this to instantiate a DAG
from airflow import DAG
# Operators; we need this to write tasks!
from airflow.operators.bash_operator import BashOperator
# This makes scheduling easy
from airflow.utils.dates import days_ago

# Task 1.1 defining DAG arguments
# You can override them on a per-task basis during operator initialization
default_args = {
    'owner': 'Azeem Jai',
    'start_date': days_ago(0),
    'email': ['jazeem@somemail.com'],
    'email_on_failure': True,
    'email_on_retry': True,
    'retries': 1,
    'retry_delay': timedelta(minutes=5),
}

# task 1.2 define the DAG
# dag = DAG(
#     dag_id='final-etl-dag',
#     default_args=default_args,
#     description='Final assignment DAG',
#     schedule_interval=timedelta(days=1),
# )
dag = DAG(
    'extract_data_dag',
    default_args=default_args,
    schedule_interval=timedelta(days=1),
    catchup=False,
    max_active_runs=1,
    tags=['example']
)

# Task 1.3 define the first task named extract
unzip_data = BashOperator(
    task_id='unzip_data',
    bash_command='tar xvzf /home/project/airflow/dags/finalassignment/tolldata.tgz -C /home/project/airflow/dags/finalassignment/staging/',
    dag=dag,
)

# task 1.4


extract_data_from_csv = BashOperator(
    task_id='extract_data_from_csv',
    bash_command='cut -d"," -f1,2,3,4 /home/project/airflow/dags/finalassignment/staging/vehicle-data.csv > /home/project/airflow/dags/finalassignment/staging/csv_data.csv',
    dag=dag
)

# task 1.5

extract_data_from_tsv = BashOperator(
    task_id='extract_data_from_tsv',
    bash_command='cut -f5,6,7 /home/project/airflow/dags/finalassignment/staging/tollplaza-data.tsv > /home/project/airflow/dags/finalassignment/staging/tsv_data.csv',
    dag=dag
)

#task 1.6 Create a task to extract data from fixed width file
extract_data_from_fixed_width = BashOperator(
    task_id='extract_data_from_fixed_width',
    bash_command='cut -c6-7 /home/project/airflow/dags/finalassignment/staging/payment-data.txt > /home/project/airflow/dags/finalassignment/staging/fixed_width_data.csv',
    dag=dag
)

# 1.7 - Create a task to consolidate data extracted from previous tasks

consolidate_data = BashOperator(
    task_id='consolidate_data',
    bash_command='paste /home/project/airflow/dags/finalassignment/staging/csv_data.csv /home/project/airflow/dags/finalassignment/staging/tsv_data.csv /home/project/airflow/dags/finalassignment/staging/fixed_width_data.csv > /home/project/airflow/dags/finalassignment/staging/extracted_data.csv',
    dag=dag
)

# Task 1.8 - Transform and load the data
transform_data = BashOperator(
    task_id='transform_data',
    bash_command='awk \'BEGIN {FS=OFS=","} { $4 = toupper($4); print }\' /home/project/airflow/dags/finalassignment/staging/extracted_data.csv > /home/project/airflow/dags/finalassignment/staging/transformed_data.csv',
    dag=dag
)

# Task 1.9 - Define the task pipeline

unzip_data >> extract_data_from_csv >> extract_data_from_tsv >> extract_data_from_fixed_width >> consolidate_data
consolidate_data >> transform_data



