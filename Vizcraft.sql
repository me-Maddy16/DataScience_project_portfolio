drop table call_logs;
CREATE TABLE call_logs (
    call_id VARCHAR(255),
    timestamp VARCHAR(255),
    call_type VARCHAR(255),
    disposition VARCHAR(255),
    talk_time VARCHAR(255),
    after_call_work_time VARCHAR(255),
    day_of_month VARCHAR(255),
    hour_of_day VARCHAR(255),
    day_of_week VARCHAR(255),
    month VARCHAR(255),
    contacted VARCHAR(255),
    handle_time VARCHAR(255),
    referral_code VARCHAR(255),
    referral_id VARCHAR(255),
    state_lob VARCHAR(255),
    referral_date VARCHAR(255),
    half_hour VARCHAR(255),
    date VARCHAR(255)
);


select * from call_logs;
drop table Demographic_Data
CREATE TABLE Demographic_Data (
    ref_id VARCHAR(255),
    member_id VARCHAR(255),
    member_age VARCHAR(255),
    member_gender VARCHAR(255),
    state_sold VARCHAR(255),
    county VARCHAR(255),
    zipcode VARCHAR(255),
    lob VARCHAR(255),
    referral_type VARCHAR(255),
    member_language VARCHAR(255)
);

select * from call_logs;
select * from Demographic_Data;

DELETE FROM call_logs
WHERE call_id = 'CALL ID';

DELETE FROM Demographic_Data
WHERE member_id = 'MEMBER ID';

CREATE TABLE new_table_name_inner AS
SELECT * FROM call_logs c
inner JOIN Demographic_Data d ON c.REFERRAL_ID = d.REF_ID;