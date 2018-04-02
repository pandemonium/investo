CREATE TABLE public.accounts (
    id integer NOT NULL,
    name text,
    brokerage_account_id text NOT NULL,
    kind text
);

CREATE SEQUENCE public.accounts_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.accounts_id_seq OWNED BY public.accounts.id;

CREATE TABLE public.currencies (
    id integer NOT NULL,
    symbol text NOT NULL,
    default_rate double precision,
    locale text DEFAULT 'sv_SE'::text
);

CREATE SEQUENCE public.currencies_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.currencies_id_seq OWNED BY public.currencies.id;

CREATE TABLE public.currency_histories (
    id integer NOT NULL,
    currency_id integer NOT NULL,
    "when" timestamp without time zone NOT NULL,
    units integer DEFAULT 1 NOT NULL,
    unit_price double precision NOT NULL
);

CREATE SEQUENCE public.currency_histories_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.currency_histories_id_seq OWNED BY public.currency_histories.id;

CREATE TABLE public.dividends (
    id integer NOT NULL,
    stock_id integer NOT NULL,
    amount double precision NOT NULL,
    ex_date date NOT NULL,
    pay_date date NOT NULL,
    currency_id integer NOT NULL,
    projected bit(1) DEFAULT B'0'::"bit" NOT NULL
);

CREATE SEQUENCE public.dividends_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.dividends_id_seq OWNED BY public.dividends.id;

CREATE TABLE public.stocks (
    id integer NOT NULL,
    symbol text NOT NULL,
    name text NOT NULL,
    "ISIN" text NOT NULL
);

CREATE SEQUENCE public.stocks_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.stocks_id_seq OWNED BY public.stocks.id;

CREATE TABLE public.transactions (
    id integer NOT NULL,
    stock_id integer NOT NULL,
    count integer NOT NULL,
    price double precision NOT NULL,
    transaction_type integer NOT NULL,
    currency_id integer NOT NULL,
    price_paid double precision NOT NULL,
    book_date date NOT NULL,
    courtage double precision NOT NULL,
    account_id integer NOT NULL
);

CREATE SEQUENCE public.transactions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE public.transactions_id_seq OWNED BY public.transactions.id;

ALTER TABLE ONLY public.currencies ALTER COLUMN id SET DEFAULT nextval('public.currencies_id_seq'::regclass);

ALTER TABLE ONLY public.currency_histories ALTER COLUMN id SET DEFAULT nextval('public.currency_histories_id_seq'::regclass);

ALTER TABLE ONLY public.dividends ALTER COLUMN id SET DEFAULT nextval('public.dividends_id_seq'::regclass);

ALTER TABLE ONLY public.stocks ALTER COLUMN id SET DEFAULT nextval('public.stocks_id_seq'::regclass);

ALTER TABLE ONLY public.transactions ALTER COLUMN id SET DEFAULT nextval('public.transactions_id_seq'::regclass);

ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT accounts_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.currencies
    ADD CONSTRAINT currencies_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.currency_histories
    ADD CONSTRAINT currency_histories_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.dividends
    ADD CONSTRAINT dividends_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.stocks
    ADD CONSTRAINT stocks_pkey PRIMARY KEY (id);

ALTER TABLE ONLY public.transactions
    ADD CONSTRAINT transactions_pkey PRIMARY KEY (id);