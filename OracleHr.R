###�ܺο��� �����ؼ� �м��� ���
getwd()    ###�ٸ�ȸ��...?
dir.create("../project180721") #.���� �ϳ� �ö� �׷� User�� �ְ���
setwd("../project180721")      # ��� ��η� ��ġ ���
list.files()
#������ ���� project:(None) Ŭ��, newprojectŬ��, existing directoryŬ��.



#######################################################
## ���� 1 rJava, DBI, RJDBC, dplyr
## ��Ű���� ȣ���Ͻÿ�
#######################################################
library(rJava)
library(DBI)
library(RJDBC)
library(XML)
library(memoise)
library(KoNLP)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rvest)
library(RColorBrewer)
library(data.table)
library(reshape)
#######################################################
## ���� 2 ����Ŭ�� Project �� �����Ͻÿ�
#######################################################
### RStudio �� Oracle ����
install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")
library(rJava)
library(DBI)
library(RJDBC)
drv <- JDBC(
  "oracle.jdbc.driver.OracleDriver",
  "C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc6.jar"
)
conn <- dbConnect(drv,
                  "jdbc:oracle:thin:@localhost:1521:xe",
                  "hr",
                  "oracle")
dbGetQuery(conn, "SELECT * FROM TAB")

#######################################################
## ���� 3 ����Ŭ�� ���̺��� ��ȸ�Ͻÿ�
####################################################### 
##�����ڵ�
tab = dbGetQuery(conn, "SELECT * FROM TAB")
tname <- tab$TNAME; tname

#######################################################
## ���� 4 ����Ŭ�� �� ���̺��� ���������������� ��ȯ�Ͻÿ�
## �������������� �̸��� ������ ���� �Ͻÿ�.
## COUNTRIES -> cnt
## DEPARTMENTS -> dep
## EMPLOYEES -> emp
## EMP_DETAILS_VIEW -> empd
## JOBS -> job
## JOB_HISTORY -> jobh
## LOCATIONS -> loc
## REGIONS -> reg
#######################################################
cnt <- data.frame(dbGetQuery(conn,"SELECT * FROM COUNTRIES"))
dep <- data.frame(dbGetQuery(conn,"SELECT * FROM DEPARTMENTS"))
emp <- data.frame(dbGetQuery(conn,"SELECT * FROM EMPLOYEES"))
empd <- data.frame(dbGetQuery(conn,"SELECT * FROM EMP_DETAILS_VIEW"))
job <- data.frame(dbGetQuery(conn,"SELECT * FROM JOBS"))
jobh <- data.frame(dbGetQuery(conn,"SELECT * FROM JOB_HISTORY"))
loc <- data.frame(dbGetQuery(conn,"SELECT * FROM LOCATIONS"))
reg <- data.frame(dbGetQuery(conn,"SELECT * FROM REGIONS"))
View(cnt)
View(dep)
View(emp)
cnt

#######################################################
## ���� 5 cnt �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�
## �������̵� = COUNTRY_ID
## ������ = COUNTRY_NAME
## �������̵� = REGION_ID
#######################################################
cnt <- cnt %>% dplyr::rename(�������̵� = COUNTRY_ID,������ = COUNTRY_NAME,�������̵� = REGION_ID)
cnt

#######################################################
## ���� 6 dep �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�
## �μ����̵� = DEPARTMENT_ID
## �μ��� = DEPARTMENT_NAME
## �Ŵ������̵� = MANAGER_ID
## ��ġ���̵� = LOCATION_ID
#######################################################
dep <- dep %>% dplyr::rename(�μ����̵� = DEPARTMENT_ID,�μ��� = DEPARTMENT_NAME,
                                  �Ŵ������̵� = MANAGER_ID, ��ġ���̵� = LOCATION_ID)
dep
#######################################################
## ���� 7 emp �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�.
## �׸��� First Name �� Last Name ��
## �ٿ��� �̸� ���� �� �÷��� �߰��Ͻÿ�
## ��, �̸� ������ ����. ex) James Dean
## �������̵� = EMPLOYEE_ID
## �̸��� = EMAIL
## ��ȭ��ȣ = PHONE_NUMBER
## ä���� = HIRE_DATE
## �������̵� = JOB_ID
## ���� = SALARY
## Ŀ�̼Ǻ��� = COMMISSION_PCT
## �Ŵ������̵� = MANAGER_ID
## �μ����̵� = DEPARTMENT_ID
#######################################################
str(emp)
emp <- emp %>% dplyr::rename(�������̵� = EMPLOYEE_ID, �̸��� = EMAIL,
                             ��ȭ��ȣ = PHONE_NUMBER, ä���� = HIRE_DATE,
                             �������̵� = JOB_ID, ���� = SALARY,
                             Ŀ�̼Ǻ��� = COMMISSION_PCT, �Ŵ������̵� = MANAGER_ID,
                             �μ����̵� = DEPARTMENT_ID) %>% 
  dplyr::mutate(�̸� = paste(emp$FIRST_NAME, emp$LAST_NAME))
head(emp)
View(emp)


#######################################################
## ���� 8  emp �� First Name �� Last Name �÷� �ΰ���
## �����Ͻÿ�. subset
#######################################################
if(is.data.frame(emp)){
  emp<- subset(emp, select= -c(FIRST_NAME,LAST_NAME))
}
View(emp)

#######################################################
## ���� 9
## �Ŵ� �����ϴ� ���޿�(���� / 12)�� �����ִ�
## ���� �̶�� �÷���
## �߰���Ű�ÿ�.(0���� ����)
#######################################################
emp <- emp %>% dplyr::mutate(����=���� %/% 12)
head(emp)

#######################################################
## ���� 10 job �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�
## �������̵� = JOB_ID
## ������ = JOB_TITLE
## �ּҿ��� = MIN_SALARY
## �ִ뿬�� = MAX_SALARY
#######################################################
job <- job %>% dplyr::rename(�������̵� = JOB_ID,������ = JOB_TITLE,
                             �ּҿ��� = MIN_SALARY,�ִ뿬�� = MAX_SALARY)
head(job)


#######################################################
## ���� 11 jobh �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�
## �������̵� = EMPLOYEE_ID
## ���������� = START_DATE
## ���������� = END_DATE
## �������̵� = JOB_ID
## �μ����̵� = DEPARTMENT_ID
#######################################################
jobh <- jobh %>% dplyr::rename(�������̵� = EMPLOYEE_ID,
                               ���������� = START_DATE,
                               ���������� = END_DATE,
                               �������̵� = JOB_ID,
                               �μ����̵� = DEPARTMENT_ID)
head(jobh)



#######################################################
## ���� 12 loc �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�
# ��ġ���̵� = LOCATION_ID
# �Ÿ��ּ� = STREET_ADDRESS
# ������ȣ = POSTAL_CODE
# ���� = CITY
# �� = STATE_PROVINCE
# �������̵� = COUNTRY_ID
#######################################################
loc <- loc %>% dplyr::rename(��ġ���̵� = LOCATION_ID,�Ÿ��ּ� = STREET_ADDRESS,
                              ������ȣ = POSTAL_CODE,���� = CITY,
                              �� = STATE_PROVINCE,�������̵� = COUNTRY_ID)
head(loc)


#######################################################
## ���� 13 reg �� �÷����� �ѱ۷� ��ȯ�Ͻÿ�
## �������̵� = REGION_ID
## ������ = REGION_NAME
#######################################################
reg <- reg %>% dplyr::rename(�������̵� = REGION_ID,
                              ������ = REGION_NAME)
head(reg)


####################################################### 

## ���� 14. ������ 10000�� �̻��� 
## ���(emp)�� ����� �̸�, �������̵�, ������ 
## ���� ������������ �����ּ���. 
####################################################### 
emp %>% dplyr::filter(���� >= 10000) %>% 
  dplyr::arrange(desc(����)) %>% 
  dplyr::select(�̸�, �������̵�, ����)


####################################################### 
## ���� 15. ������ 3000 �̸��� 
## ������� ���ʽ��� �޿��� 1%�� �����ϰڴٰ� �մϴ� 
## ������� ����� �̸�, �������̵�, ������ �����ϰ� 
## ���̵� ������������ �����ֽÿ�. �� ���ʽ����޳����� 
## ��� �̸��� ���������������� �ۼ��� �� �����Ͻÿ�. 
## ���ʽ����� �� �ݾ׿� ���������� ÷���մϴ�. ## ��Ʈ: ���ʽ�= sprintf("%0.0f ����", ����*0.01) ��� 
## ��Ʈ: rm(���ʽ����޳�����) �ϸ� rm �ϸ� �����������ӻ����� 
####################################################### 
���ʽ����޳����� <- emp %>% dplyr::filter(���� < 3000) %>% 
  dplyr::arrange(�������̵�) %>% 
  dplyr::select(�̸�, �������̵�, ����) %>%
  dplyr::mutate(���ʽ�= sprintf("%0.0f ����", ����*0.01))
head(���ʽ����޳�����)

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%20.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified
sprintf("%s�� ����", c("����","�μ�","����"))
sprintf("%s %d", "����", 1:3)

write.csv(���ʽ����޳�����,'���ʽ����޳�����.CSV')
rm(���ʽ����޳�����)

####################################################### 
## ���� 16. �����߿��� �޿��� ���� ���� ����� 
## CEO ��� �մϴ�. �̸��� �����Դϱ�? 
## apply(object, direction, function to apply) 
## ������� - 1:���ι���, 2: ���ι��� 
####################################################### 
emp %>% dplyr::filter(����==apply(emp %>% dplyr::select(����),2,max)) %>% 
  dplyr::select(�̸�)




####################################################### 
## ���� 17. ������ 10000�� �Ѵ� ������ �μ���, �̸�, 
## ������ ����Ͻÿ�. 
####################################################### 
emp %>% 
  dplyr::left_join(dep, by='�μ����̵�') %>% 
  dplyr::filter(����>=10000) %>%
  dplyr::select(�μ���, �̸�, ����) %>% 
  dplyr::arrange(desc(����))

# left�� emp, right�� dep
names(emp);
names(dep)
names(job)
# Database
# TAble <- emp + dep - �μ���, �̸�, ���� ; ����
# �ΰ� ���̺��� ���� ��ġ�� ����, �췺
# emp : �����̸Ӹ�Ű(PK) ���̵�
#         - ����Ű (FK)
#             �μ����̵�
# dep : PK �μ����̵�, firm�� pk�� fk�� ���� �ְ� �Ѵ�.
# firm : PK
# FK ����, PK�� ���� �ִ� ���̺��� ���� ���� ���̺���.
# �μ��� ���� �����ؾ���. �� ���� �͵��� ���� �� ���� �ʰ���?
# �θ��ڽİ���?! �ڽ��� �θ��� PK���� ������ �ִµ�, �װ��� fk����


hea##

######################################### 
#### [���� 18] 
#### ������ 12000 �� �Ѵ� ������ �μ���,�̸�,����,��å 
#### �� �����Ͻÿ�. 
###################################################### 
emp %>% 
  dplyr::left_join(dep,by='�μ����̵�') %>% 
  dplyr::left_join(job,by='�������̵�') %>% 
  dplyr::filter(���� >= 12000) %>% 
  dplyr::select(�μ���, �̸�, ����, ������) %>% 
  dplyr::arrange(desc(����))

##���⿡���� emp�� �ư��� �ư�.



####################################################### 
#### [���� 19] 
#### �μ��� ���� ���� ����� ���Ͻÿ� 
###############################�׷����!####################### 
�μ���_������� <- emp %>% dplyr::left_join(dep,by='�μ����̵�') %>% 
  dplyr::group_by(�μ���,�μ����̵�) %>% 
##������ �����ִ� ���� �μ��� = ����� ����, ��� �μ����̵� ����(FK)
  dplyr::summarise(�μ���������� = mean(����,na.rm=TRUE)) %>%
  dplyr::arrange(desc(�μ����������))
View(�μ���_�������)



####################################################### 
#### [���� 20] 
#### �� ȸ���� �μ� ���� �����Ͻÿ�. 
###################################################### 
dep %>% dplyr::distinct(�μ���) %>% count


####################################################### 
## ���� 21. �μ���, ����, �� �μ��������, 
## �� �μ� �� ��� ������ ��ȸ�Ѵ�. 
## ��� ������ �Ҽ��� 2 �ڸ������� ǥ���Ѵ�. 
## emp: ���� 
## dep: �μ��� 
## loc: ���� 
## [��Ʈ] left_join, group_by, summarise 
####################################################### 
aa<-dep %>% dplyr::left_join(emp,by='�μ����̵�') %>% 
  dplyr::left_join(loc,by='��ġ���̵�') %>% 
  dplyr::group_by(�μ���, ����) %>% 
  dplyr::summarise(�μ�����տ���=sprintf("%0.2f",mean(����,na.rm=T)),
                          ������=length(�������̵�)) 
View(aa)
sprintf("%0.2f",mean(����,na.rm=T))
emp$����
str(emp)

####################################################### 
#### ���� 22. �μ����� ���� ���� ������ 
#### �μ����̵�, �μ���, �ִ뿬������ 
#### ǥ�õǵ��� �ϼ���. 
###################################################### 
emp %>% dplyr::left_join(dep,by='�μ����̵�') %>% 
  dplyr::group_by(�μ����̵�, �μ���) %>% 
  dplyr::summarise(�ִ뿬�� = max(����)) 

names(emp)
names(dep)


####################################################### 
#### ���� 23. �μ����̵� �߱޹��� ������ �����Դϴ�. 
#### ������ �̸��� ����, �μ����̵� ������ ����Ͻÿ�. 
#  test <- c('apple','banana','cherry','Apple','Pineapple',NA)
# ifelse(stringr::str_detect(test,'A'),'Good','Bad')
###################################################### 
# :( 
# library(stringr)
# a<-ifelse(!is.na(emp %>% dplyr::select(�μ����̵�)),
#        emp %>% dplyr::select(�̸�, ����),"�μ����̵����")
#                   a

emp %>% dplyr::filter(!is.na(emp %>% dplyr::select(�μ����̵�))) %>% 
  dplyr::select(�̸�, ����)



####################################################### 
## ���� 24. �����߿��� �̸��� �빮�� S �� T�� ���Ե� ������ ����Ͻÿ�. 
## �����ַ� �ѱ�ϴ�. �Ʒ� apply �� �˾ƾ� �ذ��� �� �ֽ��ϴ�. 
####################################################### 
emp %>% dplyr::filter(stringr::str_detect(emp$�̸�, 'S'),
                      stringr::str_detect(emp$�̸�, 'T')) %>% 
  dplyr::select(�̸�)

"STR" == S*
  