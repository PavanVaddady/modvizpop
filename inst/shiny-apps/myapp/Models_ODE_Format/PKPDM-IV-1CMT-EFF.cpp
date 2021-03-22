$PROB IV ONE COMP PKPD EFFECT COMPARTMENT MODEL

$PARAM @annotated
CL 		: 5   : Clearance (L/hr)
V1 		: 25  : Central Volume (L)
E0 	    : 157 : Baseline Effect (units)
EC50 	: 5   : Concentration at Half-maximal Effect (mg/L)
EMAX 	: 30  : Maximum Effect (units)
KEO 	: 1.3 : Effect compartment rate constant (1/hr)

$OMEGA @annotated @block
ETA_CL      : 0.01 : ETA on CL
ETA_V1      : 0 0.01 : ETA on V1
ETA_E0      : 0 0 0.01 : ETA on E0
ETA_EC50    : 0 0 0 0.01 : ETA on EC50
ETA_EMAX    : 0 0 0 0 0.01 : ETA on EMAX
ETA_KEO		: 0 0 0 0 0 0.01 : ETA on KEO  

$SIGMA @annotated @block   
PK_ADD  : 0 : PK Additive Error 
PK_PROP : 0 0.1 : PK Proportional Error 
PD_ADD  : 0 0 0 : PD Additive Error 
PD_PROP : 0 0 0 0.01 : PD Proportional Error    

$CMT @annotated
CENT    : Central (mass)
EFF     : PD Response 

$MAIN
double CLi = CL * exp(ETA_CL);
double V1i = V1 * exp(ETA_V1);
double E0i = E0 * exp(ETA_E0);
double EC50i = EC50 * exp(ETA_EC50);
double EMAXi = EMAX * exp(ETA_EMAX);
double KEOi = KEO * exp(ETA_KEO);

$ODE
dxdt_CENT = - (CLi/V1i)*CENT;
dxdt_EFF = KEOi * (CENT - EFF);

$TABLE
double CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
double RESPONSE = (E0i - (EMAXi*EFF)/(EFF+EC50i)) * (1 + PD_PROP) + PD_ADD;

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
  
