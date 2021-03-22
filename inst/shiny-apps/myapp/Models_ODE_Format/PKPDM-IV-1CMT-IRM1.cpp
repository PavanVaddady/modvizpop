$PROB IV ONE COMP PKPD INDIRECT RESPONSE (INHIBITION OF RESPONSE INPUT) MODEL

$PARAM @annotated
CL 		: 5   : Clearance (L/hr)
V1 		: 25  : Central Volume (L)
KIN 	: 100 : Input Rate Constant (mg/hr)
KOUT 	: 10  : Output Rate Constant (1/hr)
IC50 	: 4   : Concentration at Half-maximal Effect (mg/L)
IMAX 	: 1  : Maximum Effect (units)
n 		: 1   : Emax Model Sigmoidicity 

$OMEGA @annotated @block
ETA_CL      : 0.01 : ETA on CL
ETA_V1      : 0 0.01 : ETA on V1
ETA_KIN     : 0 0 0.01 : ETA on KIN
ETA_KOUT    : 0 0 0 0.01 : ETA on KOUT
ETA_IC50    : 0 0 0 0 0.01 : ETA on EC50
ETA_IMAX    : 0 0 0 0 0 0.01 : ETA on EMAX  

$SIGMA @annotated @block   
PK_ADD  : 0 : PK Additive Error 
PK_PROP : 0 0.1 : PK Proportional Error 
PD_ADD  : 0 0 0 : PD Additive Error 
PD_PROP : 0 0 0 0.01 : PD Proportional Error    

$CMT @annotated
CENT    : Central (mass)
RESP    : PD Response 

$MAIN
double CLi = CL * exp(ETA_CL);
double V1i = V1 * exp(ETA_V1);
double KINi = KIN * exp(ETA_KIN);
double KOUTi = KOUT * exp(ETA_KOUT);
double IC50i = IC50 * exp(ETA_IC50);
double IMAXi = IMAX * exp(ETA_IMAX);

RESP_0 = KINi/KOUTi;

$ODE
dxdt_CENT = - (CLi/V1i)*CENT;
double INH = (IMAXi*pow((CENT/V1i),n))/(pow(IC50i,n)+pow((CENT/V1i),n));
dxdt_RESP = KINi*(1-INH) - KOUTi*RESP;

$TABLE
double CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
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
RESPONSE   : PD response (units)
  
