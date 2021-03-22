$PROB IV ONE COMP PKPD EMAX MODEL

$PARAM @annotated
CL 		: 5   : Clearance (L/hr)
V1 		: 25  : Central Volume (L)
E0 	    : 5   : RESPONSE at time 0 (units)
EC50 	: 10  : Concentration at Half-maximal Effect (mg/L)
EMAX 	: 10  : Maximum Effect (units)
n 		: 1   : Emax Model Sigmoidicity 

$OMEGA @annotated @block
ETA_CL      : 0.01 : ETA on CL
ETA_V1      : 0 0.01 : ETA on V1
ETA_E0      : 0 0 0.01 : ETA on E0
ETA_EC50    : 0 0 0 0.01 : ETA on EC50
ETA_EMAX    : 0 0 0 0 0.01 : ETA on EMAX  

$SIGMA @annotated @block   
PK_ADD  : 0 : PK Additive Error 
PK_PROP : 0 0.1 : PK Proportional Error 
PD_ADD  : 0 0 0 : PD Additive Error 
PD_PROP : 0 0 0 0.01 : PD Proportional Error  
   
$CMT @annotated
CENT    : Central (mass)

$MAIN
double CLi = CL * exp(ETA_CL);
double V1i = V1 * exp(ETA_V1);
double E0i = E0 * exp(ETA_E0);
double EC50i = EC50 * exp(ETA_EC50);
double EMAXi = EMAX * exp(ETA_EMAX);

$ODE
dxdt_CENT = - (CLi/V1i)*CENT;

$TABLE
double CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
double RESPONSE = E0i+(EMAXi*pow(CENT/V1i,n))/(pow(EC50i,n)+pow(CENT/V1i,n)) * (1 + PD_PROP) + PD_ADD;

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
  
