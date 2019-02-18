--
-- PostgreSQL database dump
--

-- Dumped from database version 11.1
-- Dumped by pg_dump version 11.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

ALTER TABLE ONLY public."Users_Reminders" DROP CONSTRAINT users_reminders_userid_fkey;
ALTER TABLE ONLY public."Users_Reminders" DROP CONSTRAINT users_reminders_reminderid_fkey;
ALTER TABLE ONLY public."Reminders_Emails" DROP CONSTRAINT reminders_emails_reminderid_fkey;
ALTER TABLE ONLY public."Reminders_Emails" DROP CONSTRAINT reminders_emails_emailid_fkey;
ALTER TABLE ONLY public."Users_Emails" DROP CONSTRAINT "Users_Emails_UserID_fkey";
ALTER TABLE ONLY public."Users_Emails" DROP CONSTRAINT "Users_Emails_EmailID_fkey";
ALTER TABLE ONLY public."Users_Reminders" DROP CONSTRAINT users_reminders_pkey;
ALTER TABLE ONLY public."Reminders" DROP CONSTRAINT reminders_pkey;
ALTER TABLE ONLY public."Reminders_Emails" DROP CONSTRAINT reminders_emails_pkey;
ALTER TABLE ONLY public."Users" DROP CONSTRAINT "Users_pkey";
ALTER TABLE ONLY public."Users_Emails" DROP CONSTRAINT "Users_Emails_pkey";
ALTER TABLE ONLY public."Users" DROP CONSTRAINT "Users_Email_key";
ALTER TABLE ONLY public."Emails" DROP CONSTRAINT "Emails_pkey";
ALTER TABLE ONLY public."Emails" DROP CONSTRAINT "Emails_Email_key";
ALTER TABLE public."Reminders" ALTER COLUMN "ID" DROP DEFAULT;
DROP SEQUENCE public.reminders_id_seq1;
DROP SEQUENCE public.reminders_id_seq;
DROP TABLE public."Users_Reminders";
DROP TABLE public."Users_Emails";
DROP TABLE public."Users";
DROP SEQUENCE public.user_id_seq;
DROP TABLE public."Reminders_Emails";
DROP TABLE public."Reminders";
DROP TABLE public."Emails";
DROP SEQUENCE public.email_id_seq;
--
-- Name: email_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.email_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.email_id_seq OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: Emails; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Emails" (
    "ID" integer DEFAULT nextval('public.email_id_seq'::regclass) NOT NULL,
    "Email" text
);


ALTER TABLE public."Emails" OWNER TO postgres;

--
-- Name: Reminders; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Reminders" (
    "ID" integer NOT NULL,
    "Name" character varying(100) NOT NULL,
    "Description" text NOT NULL,
    "ReminderDateTime" timestamp with time zone NOT NULL
);


ALTER TABLE public."Reminders" OWNER TO postgres;

--
-- Name: Reminders_Emails; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Reminders_Emails" (
    "ReminderID" integer NOT NULL,
    "EmailID" integer NOT NULL
);


ALTER TABLE public."Reminders_Emails" OWNER TO postgres;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_id_seq OWNER TO postgres;

--
-- Name: Users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Users" (
    "ID" integer DEFAULT nextval('public.user_id_seq'::regclass) NOT NULL,
    "Email" text
);


ALTER TABLE public."Users" OWNER TO postgres;

--
-- Name: Users_Emails; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Users_Emails" (
    "UserID" integer NOT NULL,
    "EmailID" integer NOT NULL
);


ALTER TABLE public."Users_Emails" OWNER TO postgres;

--
-- Name: Users_Reminders; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Users_Reminders" (
    "UserID" integer NOT NULL,
    "ReminderID" integer NOT NULL
);


ALTER TABLE public."Users_Reminders" OWNER TO postgres;

--
-- Name: reminders_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.reminders_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.reminders_id_seq OWNER TO postgres;

--
-- Name: reminders_id_seq1; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.reminders_id_seq1
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.reminders_id_seq1 OWNER TO postgres;

--
-- Name: reminders_id_seq1; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.reminders_id_seq1 OWNED BY public."Reminders"."ID";


--
-- Name: Reminders ID; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Reminders" ALTER COLUMN "ID" SET DEFAULT nextval('public.reminders_id_seq1'::regclass);


--
-- Data for Name: Emails; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Emails" ("ID", "Email") FROM stdin;
1	omefire@gmail.com
2	imefire@gmail.com
3	hamefire@gmail.com
4	mefired@gmail.com
5	omefire@yahoo.fr
\.


--
-- Data for Name: Reminders; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Reminders" ("ID", "Name", "Description", "ReminderDateTime") FROM stdin;
\.


--
-- Data for Name: Reminders_Emails; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Reminders_Emails" ("ReminderID", "EmailID") FROM stdin;
\.


--
-- Data for Name: Users; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Users" ("ID", "Email") FROM stdin;
1	omefire@gmail.com
2	imefire@gmail.com
3	tati@gmail.com
\.


--
-- Data for Name: Users_Emails; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Users_Emails" ("UserID", "EmailID") FROM stdin;
1	1
1	4
1	5
2	2
2	3
\.


--
-- Data for Name: Users_Reminders; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Users_Reminders" ("UserID", "ReminderID") FROM stdin;
\.


--
-- Name: email_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.email_id_seq', 5, true);


--
-- Name: reminders_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.reminders_id_seq', 1, false);


--
-- Name: reminders_id_seq1; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.reminders_id_seq1', 1, false);


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.user_id_seq', 3, true);


--
-- Name: Emails Emails_Email_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Emails"
    ADD CONSTRAINT "Emails_Email_key" UNIQUE ("Email");


--
-- Name: Emails Emails_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Emails"
    ADD CONSTRAINT "Emails_pkey" PRIMARY KEY ("ID");


--
-- Name: Users Users_Email_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users"
    ADD CONSTRAINT "Users_Email_key" UNIQUE ("Email");


--
-- Name: Users_Emails Users_Emails_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users_Emails"
    ADD CONSTRAINT "Users_Emails_pkey" PRIMARY KEY ("UserID", "EmailID");


--
-- Name: Users Users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users"
    ADD CONSTRAINT "Users_pkey" PRIMARY KEY ("ID");


--
-- Name: Reminders_Emails reminders_emails_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Reminders_Emails"
    ADD CONSTRAINT reminders_emails_pkey PRIMARY KEY ("ReminderID", "EmailID");


--
-- Name: Reminders reminders_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Reminders"
    ADD CONSTRAINT reminders_pkey PRIMARY KEY ("ID");


--
-- Name: Users_Reminders users_reminders_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users_Reminders"
    ADD CONSTRAINT users_reminders_pkey PRIMARY KEY ("UserID", "ReminderID");


--
-- Name: Users_Emails Users_Emails_EmailID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users_Emails"
    ADD CONSTRAINT "Users_Emails_EmailID_fkey" FOREIGN KEY ("EmailID") REFERENCES public."Emails"("ID");


--
-- Name: Users_Emails Users_Emails_UserID_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users_Emails"
    ADD CONSTRAINT "Users_Emails_UserID_fkey" FOREIGN KEY ("UserID") REFERENCES public."Users"("ID");


--
-- Name: Reminders_Emails reminders_emails_emailid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Reminders_Emails"
    ADD CONSTRAINT reminders_emails_emailid_fkey FOREIGN KEY ("EmailID") REFERENCES public."Emails"("ID");


--
-- Name: Reminders_Emails reminders_emails_reminderid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Reminders_Emails"
    ADD CONSTRAINT reminders_emails_reminderid_fkey FOREIGN KEY ("ReminderID") REFERENCES public."Reminders"("ID");


--
-- Name: Users_Reminders users_reminders_reminderid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users_Reminders"
    ADD CONSTRAINT users_reminders_reminderid_fkey FOREIGN KEY ("ReminderID") REFERENCES public."Reminders"("ID");


--
-- Name: Users_Reminders users_reminders_userid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Users_Reminders"
    ADD CONSTRAINT users_reminders_userid_fkey FOREIGN KEY ("UserID") REFERENCES public."Users"("ID");


--
-- PostgreSQL database dump complete
--

