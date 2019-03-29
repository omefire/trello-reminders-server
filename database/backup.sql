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
DROP INDEX public.reminderdatetime_idx;
ALTER TABLE ONLY public."Users_Reminders" DROP CONSTRAINT users_reminders_pkey;
ALTER TABLE ONLY public."Reminders" DROP CONSTRAINT reminders_pkey;
ALTER TABLE ONLY public."Reminders_Emails" DROP CONSTRAINT reminders_emails_pkey;
ALTER TABLE ONLY public."Users" DROP CONSTRAINT "Users_pkey";
ALTER TABLE ONLY public."Users_Emails" DROP CONSTRAINT "Users_Emails_pkey";
ALTER TABLE ONLY public."Users" DROP CONSTRAINT "Users_Email_key";
ALTER TABLE ONLY public."Tokens" DROP CONSTRAINT "Tokens_pkey";
ALTER TABLE ONLY public."Tokens" DROP CONSTRAINT "Tokens_TrelloID_key";
ALTER TABLE ONLY public."Emails" DROP CONSTRAINT "Emails_pkey";
ALTER TABLE ONLY public."Emails" DROP CONSTRAINT "Emails_Email_key";
ALTER TABLE public."Reminders" ALTER COLUMN "ID" DROP DEFAULT;
DROP SEQUENCE public.reminders_id_seq1;
DROP SEQUENCE public.reminders_id_seq;
DROP TABLE public."Users_Reminders";
DROP TABLE public."Users_Emails";
DROP TABLE public."Users";
DROP SEQUENCE public.user_id_seq;
DROP TABLE public."Tokens";
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
    "ReminderDateTime" timestamp without time zone NOT NULL,
    "Processed" boolean DEFAULT false NOT NULL
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
-- Name: Tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public."Tokens" (
    "TrelloID" character varying NOT NULL,
    "Token" character varying NOT NULL
);


ALTER TABLE public."Tokens" OWNER TO postgres;

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
4	mefired@gmail.com
5	omefire@yahoo.fr
3	hamidmefire@gmail.com
\.


--
-- Data for Name: Reminders; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Reminders" ("ID", "Name", "Description", "ReminderDateTime", "Processed") FROM stdin;
6	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
7	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
9	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
10	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
11	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
16	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
17	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
18	ree	ABCDESC	2016-12-09 10:04:26.349858	t
19	ree	ABCDESC	2016-12-09 10:04:26.349858	t
20	ree	ABCDESC	2016-12-09 10:04:26.349858	t
22	ree	ABCDESC	2016-12-09 10:04:26.349858	t
23	ree	ABCDESC	2016-12-09 10:04:26.349858	t
24	ree	ABCDESC	2016-12-09 10:04:26.349858	t
25	test	testd	2020-12-31 11:59:00	t
26	kiliku	testd	2020-12-31 11:59:00	t
27	kiliku	testd	2020-12-31 11:59:00	t
28	Reminder123	Reminder123	2025-12-31 11:59:00	t
29	YKD	YKD	2030-12-01 04:47:00	t
30	YKD	YKD	2030-12-01 04:47:00	t
31	test	testd	2020-12-31 11:59:00	t
32	tk	tkd	2020-03-23 23:01:00	t
33	tk	tkd	2020-03-23 23:01:00	t
34	er	erd	2020-07-05 09:06:00	t
35	ewd	ewd	2020-09-14 23:03:00	t
36	wert	wet	2020-12-31 11:59:00	t
37	wer	asef	2020-12-31 11:59:00	t
38	awert	awet	2020-12-31 11:59:00	t
39	awegt	awet	2020-12-31 11:59:00	t
40	asdf	asdf	2020-12-31 11:59:00	t
41	oiu	oiu	2020-12-31 11:59:00	t
42	w4er	3245	2020-08-31 11:57:00	t
43	w4er	3245	2020-08-31 11:57:00	t
44	ry	erty	2020-12-31 11:59:00	t
45	wert	wer	2020-12-31 11:59:00	t
46	adsf	asdf	2020-12-31 11:59:00	t
47	asdf	asdf	2020-12-31 11:59:00	t
48	rh	ewrt	2020-12-31 11:59:00	t
49	ert	qwet	2020-12-31 11:59:00	t
50	awer	wer	2020-12-31 11:59:00	t
51	awer	wer	2020-12-31 11:59:00	t
52	awer	wer	2020-12-31 11:59:00	t
53	awer	wer	2020-12-31 11:59:00	t
54	awer	wer	2020-12-31 11:59:00	t
55	eryt34y	34y34y	2020-12-31 11:59:00	t
56	ery	aerh	2020-12-31 11:59:00	t
57	ery	aerh	2020-12-31 11:59:00	t
58	ery	aerh	2020-12-31 11:59:00	t
59	ery	aerh	2020-12-31 11:59:00	t
60	asdf	asdf	2020-12-31 11:59:00	t
61	asdf	asdf	2020-12-31 11:59:00	t
62	asdf	asdf	2020-12-31 11:59:00	t
63	rty	rty	2020-12-31 11:59:00	t
64	rty	rty	2020-12-31 11:59:00	t
65	oiu	oiu	2020-12-31 11:58:00	t
66	oiu	oiu	2020-12-31 11:58:00	t
67	Rikilik	asdf	2020-12-30 23:59:00	t
68	asdf	waeg	2020-12-31 11:59:00	t
69	TZ	TZ	2019-03-17 07:00:00	t
70	Acheter les medicaments de Khayla	Acheter les medicaments de Khayla	2019-03-17 07:00:00	t
71	ERR	ERR	2019-03-17 07:00:00	t
2	ABC	ABCDESC	2019-03-19 16:19:26.349858	t
1	ABC	ABCDESC	2019-03-27 08:09:26.349858	t
4	ABC	ABCDESC	2016-12-09 10:04:26.349858	t
72	MI	MI	2019-03-29 10:58:00	f
3	Brainstorm SEO ideas for Trello Reminders	Get the team together to brainstorm ideas on how to successfully run an SEO campaign and therefore improve our results on Google search for target keywords [Trello reminders, Trello notifications, etc....]	2019-03-29 10:50:00	t
73	FOX	FOX TEST	2016-12-09 15:04:26.349858	f
74	FOX	FOX TEST	2016-12-09 15:04:26.349858	f
75	FOXY	FOX TEST	2016-12-09 15:04:26.349858	f
\.


--
-- Data for Name: Reminders_Emails; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Reminders_Emails" ("ReminderID", "EmailID") FROM stdin;
16	1
17	1
18	1
19	1
20	1
22	1
23	1
24	1
25	1
26	1
27	1
28	1
28	4
28	5
29	4
29	5
30	4
30	5
31	1
31	5
32	1
32	5
33	1
33	5
34	1
35	1
36	1
37	1
38	1
39	1
40	1
41	1
42	1
43	1
44	1
45	1
46	1
47	1
48	5
49	1
50	1
51	1
52	1
53	1
54	1
55	1
56	1
57	1
58	1
59	1
60	4
61	4
62	4
63	1
64	1
65	1
66	1
67	1
68	1
69	1
70	1
71	4
1	1
3	1
4	1
3	3
73	3
74	3
75	4
\.


--
-- Data for Name: Tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public."Tokens" ("TrelloID", "Token") FROM stdin;
abc	my-token
abcd	my-token
123my_trello_id	aa01de3b944af8b426a981d1cb1a1113cad8ee686f7f315c5d72f0eb8eef9068
1255my_trello_id	aa01de3b944af8b426a981d1cb1a1113cad8ee686f7f315c5d72f0eb8eef9068
566fe951e7f0d76ceb789d6e	aa01de3b944af8b426a981d1cb1a1113cad8ee686f7f315c5d72f0eb8eef9068
5c86631092f28249c6447290	1ccf4919f03a4803b8865e6de4ff756404f03d9836a4bc664fd2f212db4438a1
efg	aa01de3b944af8b426a981d1cb1a1113cad8ee686f7f315c5d72f0eb8eef9068
efgf	efghijklmnopqrstuvw
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
1	6
1	7
1	9
1	10
1	11
1	16
1	17
1	18
1	19
1	20
1	22
1	23
1	24
1	25
1	26
1	27
1	28
1	29
1	30
1	31
1	32
1	33
1	34
1	35
1	36
1	37
1	38
1	39
1	40
1	41
1	42
1	43
1	44
1	45
1	46
1	47
1	48
1	49
1	50
1	51
1	52
1	53
1	54
1	55
1	56
1	57
1	58
1	59
1	60
1	61
1	62
1	63
1	64
1	65
1	66
1	67
1	68
1	69
1	70
1	71
1	73
1	74
2	75
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

SELECT pg_catalog.setval('public.reminders_id_seq1', 75, true);


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
-- Name: Tokens Tokens_TrelloID_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Tokens"
    ADD CONSTRAINT "Tokens_TrelloID_key" UNIQUE ("TrelloID");


--
-- Name: Tokens Tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public."Tokens"
    ADD CONSTRAINT "Tokens_pkey" PRIMARY KEY ("TrelloID", "Token");


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
-- Name: reminderdatetime_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX reminderdatetime_idx ON public."Reminders" USING btree ("ReminderDateTime");


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

