$PROB ABS TWO COMP PKPD INDIRECT RESPONSE (STIMULATION OF RESPONSE PRODUCTION) MODEL

$PARAM @annotated
KA 		: 0.5 : Absorption Rate Constant (1/hr)
CL 		: 5   : Clearance (L/hr)
Q 		: 2.5 : Intercompartmental Clearance (L/hr)
V1 		: 25  : Central Volume (L)
V2 		: 50  : Peripheral Volume (L)
F1 		: 0.8 : Bioavailability (unitless)
LAGTIME : 0   : Lag Time (hr)
KIN 	: 100 : Input Rate Constant (mg/hr)
KOUT 	: 10  : Output Rate Constant (1/hr)
EC50 	: 2   : Concentration at Half-maximal Effect (mg/L)
EMAX 	: 1   : Maximum Effect (units)
n 		: 1   : Emax Model Sigmoidicity 

$OMEGA @annotated @block
ETA_KA      : 0.01 : ETA on KA
ETA_CL      : 0 0.01 : ETA on CL
ETA_Q       : 0 0 0.01 : ETA on Q
ETA_V1      : 0 0 0 0.01 : ETA on V1
ETA_V2      : 0 0 0 0 0.01 : ETA on V2
ETA_F1      : 0 0 0 0 0 0.01 : ETA on F1
ETA_LAGTIME : 0 0 0 0 0 0 0.01 : ETA on LAGTIME 
ETA_KIN     : 0 0 0 0 0 0 0 0.01 : ETA on KIN
ETA_KOUT    : 0 0 0 0 0 0 0 0 0.01 : ETA on KOUT
ETA_EC50    : 0 0 0 0 0 0 0 0 0 0.01 : ETA on EC50
ETA_EMAX    : 0 0 0 0 0 0 0 0 0 0 0.01 : ETA on EMAX  

$SIGMA @annotated @block   
PK_ADD  : 0 : PK Additive Error 
PK_PROP : 0 0.1 : PK Proportional Error 
PD_ADD  : 0 0 0 : PD Additive Error 
PD_PROP : 0 0 0 0.01 : PD Proportional Error    

$CMT @annotated
GUT     : Extravascular (mass)
CENT    : Central (mass)
PERIPH  : Peripheral (mass)
RESP    : PD Response 

$MAIN
double KAi = KA * exp(ETA_KA);
double CLi = CL * exp(ETA_CL);
double Qi = Q * exp(ETA_Q);
double V1i = V1 * exp(ETA_V1);
double V2i = V2 * exp(ETA_V2);
double F1i = F1 * exp(ETA_F1);
double LAGTIMEi = LAGTIME * exp(ETA_LAGTIME);
double KINi = KIN * exp(ETA_KIN);
double KOUTi = KOUT * exp(ETA_KOUT);
double EC50i = EC50 * exp(ETA_EC50);
double EMAXi = EMAX * exp(ETA_EMAX);

_F(1) = F1i;
_ALAG(1) = LAGTIMEi;
RESP_0 = KINi/KOUTi;

$ODE
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (CLi/V1i)*CENT - (Qi/V1i)*CENT + (Qi/V2i)*PERIPH;
dxdt_PERIPH = (Qi/V1i)*CENT - (Qi/V2i)*PERIPH;
double STIM = (EMAXi*pow((CENT/V1i),n))/(pow(EC50i,n)+pow((CENT/V1i),n));
dxdt_RESP = KINi*(1+STIM) - KOUTi*RESP;

$TABLE
double CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
double PERIPHERAL = PERIPH/V2i * (1 + PK_PROP) + PK_ADD;
double RESPONSE = RESP * (1 + PD_PROP) + PD_ADD;

int i = 1;
while(CENTRAL < 0 & i <= 50) {
	simeps();
	CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
	i++;
}

if(i > 50) mrg::report("Positive concentrations could not be obtained.");

$CAPTURE @annotated
CENTRAL    : Plasma Concentration (mass/volume)
PERIPHERAL : Conc in Peripheral CMT (mass/volume) 
RESPONSE   : PD response (units)
  
