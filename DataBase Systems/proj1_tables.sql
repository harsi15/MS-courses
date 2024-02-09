DROP TABLE grades;
DROP TABLE enrollments CASCADE CONSTRAINTS;
DROP TABLE classes;
DROP TABLE course_credit;
DROP TABLE courses CASCADE CONSTRAINTS;
DROP TABLE students CASCADE CONSTRAINTS;

CREATE TABLE students (
	sid char(4) PRIMARY KEY CHECK (sid LIKE 'B%'),
	firstname varchar2(15) NOT NULL,
	lastname varchar2(15) NOT NULL, 
	status varchar2(10) CHECK (status IN ('freshman', 'sophomore', 'junior', 'senior', 'graduate')), 
	gpa NUMBER(3,2) CHECK (gpa BETWEEN 0 AND 4.0),
	email varchar2(20) UNIQUE
);

CREATE TABLE courses (
	dept_code varchar2(4) NOT NULL, 
	course# number(3) NOT NULL CHECK (course# BETWEEN 100 AND 799), 
	title varchar2(20) NOT NULL,
	PRIMARY KEY (dept_code, course#)
);

CREATE TABLE course_credit (
	course# number(3) NOT NULL primary KEY CHECK (course# BETWEEN 100 AND 799),
	credits number(1) CHECK (credits IN (3, 4)),
	CHECK ((course# < 500 AND credits = 4) OR (course# >= 500 AND credits = 3))
);

CREATE TABLE classes (
	classid char(5) PRIMARY KEY CHECK (classid like 'c%'),
	dept_code varchar2(4) NOT NULL,
	course# number(3) NOT NULL,
	sect# number(2), 
	year number(4),
	semester varchar2(6) CHECK (semester IN ('Spring', 'Fall', 'Summer')), 
	limit number(3),
	class_size number(3),
	FOREIGN KEY (dept_code, course#) REFERENCES courses ON DELETE CASCADE, 
	UNIQUE(dept_code, course#, sect#, year, semester),
	CHECK (class_size <= limit)
);

CREATE TABLE enrollments (
	sid char(4) REFERENCES students,
	classid char(5) REFERENCES classes, 
	lgrade char CHECK (lgrade IN ('A', 'B', 'C', 'D', 'F', 'I', NULL)),
	PRIMARY KEY (sid, classid)
);

CREATE TABLE grades(
	lgrade char CHECK (lgrade IN ('A', 'B', 'C', 'D', 'F', 'I', NULL)),
	ngrade number(1) CHECK (ngrade IN (4, 3, 2, 1, 0, NULL)),
	CHECK ((lgrade = 'A' AND ngrade = 4) OR (lgrade = 'B' AND ngrade = 3)
	OR (lgrade = 'C' AND ngrade = 2) 
	OR (lgrade = 'D' AND ngrade = 1) 
	OR (lgrade = 'F' AND ngrade = 0)
	OR (lgrade = 'I' AND ngrade = NULL)
	OR (lgrade = NULL AND ngrade = NULL))
);

INSERT INTO students VALUES ('B001', 'Anne', 'Broder', 'junior', 3.6, 'broder@bu.edu');
INSERT INTO students VALUES ('B002', 'Terry', 'Buttler', 'senior', 3.7, 'buttler@bu.edu');
INSERT INTO students VALUES ('B003', 'Tracy', 'Wang', 'senior', 4.0, 'wang@bu.edu');
INSERT INTO students VALUES ('B004', 'Barbara', 'Callan', 'junior', 2.5, 'callan@bu.edu');
INSERT INTO students VALUES ('B005', 'Jack', 'Smith', 'graduate', 3.0, 'smith@bu.edu');
INSERT INTO students VALUES ('B006', 'Terry', 'Zillman', 'graduate', 4.0, 'zillman@bu.edu');
INSERT INTO students VALUES ('B007', 'Becky', 'Lee', 'senior', 4.0, 'lee@bu.edu');
INSERT INTO students VALUES ('B008', 'Tom', 'Baker', 'freshman', NULL, 'baker@bu.edu');
INSERT INTO students VALUES ('B009', 'Raju', 'Uppalapati', 'graduate', NULL, 'uppalapati@bu.edu');

INSERT INTO courses VALUES ('CS', 432, 'database systems');
INSERT INTO courses VALUES ('Math', 314, 'discrete math');
INSERT INTO courses VALUES ('CS', 240, 'data structure');
INSERT INTO courses VALUES ('Math', 221, 'calculus I');
INSERT INTO courses VALUES ('CS', 532, 'database systems');
INSERT INTO courses VALUES ('CS', 552, 'operating systems');
INSERT INTO courses VALUES ('CS', 557, 'distributed systems');
INSERT INTO courses VALUES ('BIOL', 425, 'molecular biology');
INSERT INTO courses VALUES ('CS', 547 , 'mobile Systems');
INSERT INTO courses VALUES ('CS', 535 , 'data mining');
INSERT INTO courses VALUES ('CS', 524 , 'mobile Robotics');

INSERT INTO course_credit VALUES (432, 4);
INSERT INTO course_credit VALUES (314, 4);
INSERT INTO course_credit VALUES (240, 4);
INSERT INTO course_credit VALUES (221, 4);
INSERT INTO course_credit VALUES (532, 3);
INSERT INTO course_credit VALUES (552, 3);
INSERT INTO course_credit VALUES (425, 4);
INSERT INTO course_credit VALUES (557, 3);
INSERT INTO course_credit VALUES (547, 3);
INSERT INTO course_credit VALUES (535, 3);
INSERT INTO course_credit VALUES (524, 3);

INSERT INTO classes VALUES  ('c0001', 'CS', 432, 1, 2022, 'Spring', 35, 34);
INSERT INTO classes VALUES  ('c0002', 'Math', 314, 1, 2022, 'Spring', 25, 24);
INSERT INTO classes VALUES  ('c0003', 'Math', 314, 2, 2022, 'Spring', 25, 22);
INSERT INTO classes VALUES  ('c0004', 'CS', 432, 2, 2022, 'Spring', 30, 30);
INSERT INTO classes VALUES  ('c0005', 'CS', 240, 1, 2022, 'Fall', 40, 39);
INSERT INTO classes VALUES  ('c0006', 'CS', 532, 1, 2022, 'Spring', 29, 28);
INSERT INTO classes VALUES  ('c0007', 'Math', 221, 1, 2022, 'Spring', 30, 30);
INSERT INTO classes VALUES  ('c0008', 'CS', 557, 1, 2024, 'Spring', 40, 39);
INSERT INTO classes VALUES  ('c0009', 'CS', 547, 1, 2023, 'Fall', 20, 20);
INSERT INTO classes VALUES  ('c0010', 'CS', 524, 1, 2023, 'Fall', 30, 29);
INSERT INTO classes VALUES  ('c0011', 'CS', 535, 1, 2024, 'Spring', 60, 60);
INSERT INTO classes VALUES  ('c0012', 'Math', 221, 1, 2024, 'Spring', 30, 30);
INSERT INTO classes VALUES  ('c0013', 'CS', 532, 1, 2024, 'Spring', 29, 28);

INSERT INTO enrollments VALUES  ('B001', 'c0001', 'A');
INSERT INTO enrollments VALUES  ('B002', 'c0002', 'B');
INSERT INTO enrollments VALUES  ('B003', 'c0004', 'A');
INSERT INTO enrollments VALUES  ('B003', 'c0001', 'A');
INSERT INTO enrollments VALUES  ('B004', 'c0004', 'C');
INSERT INTO enrollments VALUES  ('B004', 'c0005', 'B');
INSERT INTO enrollments VALUES  ('B005', 'c0006', 'B');
INSERT INTO enrollments VALUES  ('B006', 'c0006', 'A');
INSERT INTO enrollments VALUES  ('B006', 'c0008', 'A');
INSERT INTO enrollments VALUES  ('B001', 'c0002', 'C');
INSERT INTO enrollments VALUES  ('B003', 'c0005', NULL);
INSERT INTO enrollments VALUES  ('B007', 'c0007', 'A');
INSERT INTO enrollments VALUES  ('B005', 'c0004', 'B');
INSERT INTO enrollments VALUES  ('B002', 'c0006', 'A');
INSERT INTO enrollments VALUES  ('B003', 'c0006', 'A');
INSERT INTO enrollments VALUES  ('B007', 'c0006', 'A');
INSERT INTO enrollments VALUES  ('B001', 'c0006', 'B');
INSERT INTO enrollments VALUES  ('B002', 'c0004', 'A');
INSERT INTO enrollments VALUES  ('B001', 'c0005', 'B');
INSERT INTO enrollments VALUES  ('B007', 'c0008', 'A');
INSERT INTO enrollments VALUES  ('B005', 'c0005', NULL);
INSERT INTO enrollments VALUES  ('B009', 'c0010', 'A');
INSERT INTO enrollments VALUES  ('B002', 'c0009', 'A');
INSERT INTO enrollments VALUES  ('B007', 'c0011', 'A');
INSERT INTO enrollments VALUES  ('B005', 'c0011', 'B');
INSERT INTO enrollments VALUES  ('B006', 'c0009', 'A');
INSERT INTO enrollments VALUES  ('B009', 'c0013', 'A');

INSERT INTO grades VALUES  ('A', 4);
INSERT INTO grades VALUES  ('B', 3);
INSERT INTO grades VALUES  ('C', 2);
INSERT INTO grades VALUES  ('D', 1);
INSERT INTO grades VALUES  ('F', 0);
INSERT INTO grades VALUES  ('I', NULL);