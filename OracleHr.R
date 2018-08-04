###외부에서 젒복해서 분석할 경우
getwd()    ###다른회사...?
dir.create("../project180721") #.으로 하나 올라가 그럼 User에 있겠저
setwd("../project180721")      # 상대 경로로 위치 잡기
list.files()
#오른쪽 위에 project:(None) 클릭, newproject클릭, existing directory클릭.



#######################################################
## 문제 1 rJava, DBI, RJDBC, dplyr
## 패키지를 호출하시오
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
## 문제 2 오라클과 Project 를 연결하시오
#######################################################
### RStudio 와 Oracle 연결
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
## 문제 3 오라클의 테이블을 조회하시오
####################################################### 
##접속코드
tab = dbGetQuery(conn, "SELECT * FROM TAB")
tname <- tab$TNAME; tname

#######################################################
## 문제 4 오라클의 각 테이블을 데이터프레임으로 전환하시오
## 데이터프레임의 이름은 다음과 같이 하시오.
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
## 문제 5 cnt 의 컬럼명을 한글로 전환하시오
## 국가아이디 = COUNTRY_ID
## 국가명 = COUNTRY_NAME
## 지역아이디 = REGION_ID
#######################################################
cnt <- cnt %>% dplyr::rename(국가아이디 = COUNTRY_ID,국가명 = COUNTRY_NAME,지역아이디 = REGION_ID)
cnt

#######################################################
## 문제 6 dep 의 컬럼명을 한글로 전환하시오
## 부서아이디 = DEPARTMENT_ID
## 부서명 = DEPARTMENT_NAME
## 매니저아이디 = MANAGER_ID
## 위치아이디 = LOCATION_ID
#######################################################
dep <- dep %>% dplyr::rename(부서아이디 = DEPARTMENT_ID,부서명 = DEPARTMENT_NAME,
                                  매니저아이디 = MANAGER_ID, 위치아이디 = LOCATION_ID)
dep
#######################################################
## 문제 7 emp 의 컬럼명을 한글로 전환하시오.
## 그리고 First Name 과 Last Name 을
## 붙여서 이름 으로 된 컬럼을 추가하시오
## 단, 이름 간격은 띄울것. ex) James Dean
## 직원아이디 = EMPLOYEE_ID
## 이메일 = EMAIL
## 전화번호 = PHONE_NUMBER
## 채용일 = HIRE_DATE
## 업무아이디 = JOB_ID
## 연봉 = SALARY
## 커미션비율 = COMMISSION_PCT
## 매니저아이디 = MANAGER_ID
## 부서아이디 = DEPARTMENT_ID
#######################################################
str(emp)
emp <- emp %>% dplyr::rename(직원아이디 = EMPLOYEE_ID, 이메일 = EMAIL,
                             전화번호 = PHONE_NUMBER, 채용일 = HIRE_DATE,
                             업무아이디 = JOB_ID, 연봉 = SALARY,
                             커미션비율 = COMMISSION_PCT, 매니저아이디 = MANAGER_ID,
                             부서아이디 = DEPARTMENT_ID) %>% 
  dplyr::mutate(이름 = paste(emp$FIRST_NAME, emp$LAST_NAME))
head(emp)
View(emp)


#######################################################
## 문제 8  emp 의 First Name 과 Last Name 컬럼 두개를
## 삭제하시오. subset
#######################################################
if(is.data.frame(emp)){
  emp<- subset(emp, select= -c(FIRST_NAME,LAST_NAME))
}
View(emp)

#######################################################
## 문제 9
## 매달 지급하는 월급여(연봉 / 12)를 보여주는
## 월급 이라는 컬럼을
## 추가시키시오.(0단위 절삭)
#######################################################
emp <- emp %>% dplyr::mutate(월급=연봉 %/% 12)
head(emp)

#######################################################
## 문제 10 job 의 컬럼명을 한글로 전환하시오
## 업무아이디 = JOB_ID
## 업무명 = JOB_TITLE
## 최소연봉 = MIN_SALARY
## 최대연봉 = MAX_SALARY
#######################################################
job <- job %>% dplyr::rename(업무아이디 = JOB_ID,업무명 = JOB_TITLE,
                             최소연봉 = MIN_SALARY,최대연봉 = MAX_SALARY)
head(job)


#######################################################
## 문제 11 jobh 의 컬럼명을 한글로 전환하시오
## 직원아이디 = EMPLOYEE_ID
## 업무시작일 = START_DATE
## 업무종료일 = END_DATE
## 업무아이디 = JOB_ID
## 부서아이디 = DEPARTMENT_ID
#######################################################
jobh <- jobh %>% dplyr::rename(직원아이디 = EMPLOYEE_ID,
                               업무시작일 = START_DATE,
                               업무종료일 = END_DATE,
                               업무아이디 = JOB_ID,
                               부서아이디 = DEPARTMENT_ID)
head(jobh)



#######################################################
## 문제 12 loc 의 컬럼명을 한글로 전환하시오
# 위치아이디 = LOCATION_ID
# 거리주소 = STREET_ADDRESS
# 우편번호 = POSTAL_CODE
# 도시 = CITY
# 주 = STATE_PROVINCE
# 국가아이디 = COUNTRY_ID
#######################################################
loc <- loc %>% dplyr::rename(위치아이디 = LOCATION_ID,거리주소 = STREET_ADDRESS,
                              우편번호 = POSTAL_CODE,도시 = CITY,
                              주 = STATE_PROVINCE,국가아이디 = COUNTRY_ID)
head(loc)


#######################################################
## 문제 13 reg 의 컬럼명을 한글로 전환하시오
## 지역아이디 = REGION_ID
## 지역명 = REGION_NAME
#######################################################
reg <- reg %>% dplyr::rename(지역아이디 = REGION_ID,
                              지역명 = REGION_NAME)
head(reg)


####################################################### 

## 문제 14. 연봉이 10000불 이상인 
## 사원(emp)의 목록을 이름, 직원아이디, 연봉을 
## 연봉 내림차순으로 보여주세요. 
####################################################### 
emp %>% dplyr::filter(연봉 >= 10000) %>% 
  dplyr::arrange(desc(연봉)) %>% 
  dplyr::select(이름, 직원아이디, 연봉)


####################################################### 
## 문제 15. 연봉이 3000 미만인 
## 사원에게 보너스로 급여의 1%를 지급하겠다고 합니다 
## 대상자의 목록을 이름, 직원아이디, 연봉을 기재하고 
## 아이디 오름차순으로 보여주시오. 단 보너스지급내역서 
## 라는 이름의 데이터프레임으로 작성한 후 삭제하시오. 
## 보너스에는 각 금액에 만원단위를 첨부합니다. ## 힌트: 보너스= sprintf("%0.0f 만원", 연봉*0.01) 사용 
## 힌트: rm(보너스지급내역서) 하면 rm 하면 데이터프레임삭제됨 
####################################################### 
보너스지급내역서 <- emp %>% dplyr::filter(연봉 < 3000) %>% 
  dplyr::arrange(직원아이디) %>% 
  dplyr::select(이름, 직원아이디, 연봉) %>%
  dplyr::mutate(보너스= sprintf("%0.0f 만원", 연봉*0.01))
head(보너스지급내역서)

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%20.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified
sprintf("%s아 놀자", c("혜민","민선","예리"))
sprintf("%s %d", "문제", 1:3)

write.csv(보너스지급내역서,'보너스지급내역서.CSV')
rm(보너스지급내역서)

####################################################### 
## 문제 16. 직원중에서 급여가 가장 높은 사람이 
## CEO 라고 합니다. 이름이 무엇입니까? 
## apply(object, direction, function to apply) 
## 적용방향 - 1:가로방향, 2: 세로방향 
####################################################### 
emp %>% dplyr::filter(연봉==apply(emp %>% dplyr::select(연봉),2,max)) %>% 
  dplyr::select(이름)




####################################################### 
## 문제 17. 연봉이 10000이 넘는 직원의 부서명, 이름, 
## 연봉을 출력하시오. 
####################################################### 
emp %>% 
  dplyr::left_join(dep, by='부서아이디') %>% 
  dplyr::filter(연봉>=10000) %>%
  dplyr::select(부서명, 이름, 연봉) %>% 
  dplyr::arrange(desc(연봉))

# left가 emp, right가 dep
names(emp);
names(dep)
names(job)
# Database
# TAble <- emp + dep - 부서명, 이름, 연봉 ; 조인
# 두개 테이블을 먼저 합치고 나서, 쎌렉
# emp : 프라이머리키(PK) 아이디
#         - 포린키 (FK)
#             부서아이디
# dep : PK 부서아이디, firm의 pk를 fk로 갖고 있게 한다.
# firm : PK
# FK 없이, PK만 갖고 있는 테이블은 제일 상위 테이블임.
# 부서가 먼저 존재해야지. 그 다음 것들이 있을 수 있지 않겠음?
# 부모자식관계?! 자식이 부모의 PK값을 가지고 있는데, 그것을 fk라함


hea##

######################################### 
#### [문제 18] 
#### 연봉이 12000 이 넘는 직원의 부서명,이름,연봉,직책 
#### 을 기재하시오. 
###################################################### 
emp %>% 
  dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::left_join(job,by='업무아이디') %>% 
  dplyr::filter(연봉 >= 12000) %>% 
  dplyr::select(부서명, 이름, 연봉, 업무명) %>% 
  dplyr::arrange(desc(연봉))

##여기에서는 emp가 아가지 아가.



####################################################### 
#### [문제 19] 
#### 부서명 별로 연봉 평균을 구하시오 
###############################그룹빠이!####################### 
부서별_연봉평균 <- emp %>% dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::group_by(부서명,부서아이디) %>% 
##밖으로 보여주는 것은 부서명 = 명목상 기준, 사실 부서아이디가 기준(FK)
  dplyr::summarise(부서별연봉평균 = mean(연봉,na.rm=TRUE)) %>%
  dplyr::arrange(desc(부서별연봉평균))
View(부서별_연봉평균)



####################################################### 
#### [문제 20] 
#### 이 회사의 부서 수를 기재하시오. 
###################################################### 
dep %>% dplyr::distinct(부서명) %>% count


####################################################### 
## 문제 21. 부서명, 도시, 각 부서별사원수, 
## 각 부서 별 평균 연봉을 조회한다. 
## 평균 연봉은 소수점 2 자리까지만 표현한다. 
## emp: 연봉 
## dep: 부서명 
## loc: 도시 
## [힌트] left_join, group_by, summarise 
####################################################### 
aa<-dep %>% dplyr::left_join(emp,by='부서아이디') %>% 
  dplyr::left_join(loc,by='위치아이디') %>% 
  dplyr::group_by(부서명, 도시) %>% 
  dplyr::summarise(부서별평균연봉=sprintf("%0.2f",mean(연봉,na.rm=T)),
                          직원수=length(직원아이디)) 
View(aa)
sprintf("%0.2f",mean(연봉,na.rm=T))
emp$연봉
str(emp)

####################################################### 
#### 문제 22. 부서벼로 가장 높은 연봉을 
#### 부서아이디, 부서명, 최대연봉으로 
#### 표시되도록 하세요. 
###################################################### 
emp %>% dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::group_by(부서아이디, 부서명) %>% 
  dplyr::summarise(최대연봉 = max(연봉)) 

names(emp)
names(dep)


####################################################### 
#### 문제 23. 부서아이디를 발급받지 않으면 신입입니다. 
#### 신입의 이름과 연봉, 부서아이디 없음을 출력하시오. 
#  test <- c('apple','banana','cherry','Apple','Pineapple',NA)
# ifelse(stringr::str_detect(test,'A'),'Good','Bad')
###################################################### 
# :( 
# library(stringr)
# a<-ifelse(!is.na(emp %>% dplyr::select(부서아이디)),
#        emp %>% dplyr::select(이름, 연봉),"부서아이디없음")
#                   a

emp %>% dplyr::filter(!is.na(emp %>% dplyr::select(부서아이디))) %>% 
  dplyr::select(이름, 연봉)



####################################################### 
## 문제 24. 직원중에서 이름에 대문자 S 와 T가 포함된 직원을 출력하시오. 
## 다음주로 넘깁니다. 아래 apply 를 알아야 해결할 수 있습니다. 
####################################################### 
emp %>% dplyr::filter(stringr::str_detect(emp$이름, 'S'),
                      stringr::str_detect(emp$이름, 'T')) %>% 
  dplyr::select(이름)

"STR" == S*
  