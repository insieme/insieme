
typedef struct jpc_mqstate_s {
    
    struct jpc_mqstate_s *nlps;
    
} jpc_mqstate_t;

jpc_mqstate_t jpc_mqstates[3] = {
    {&jpc_mqstates[1]}
};

int main() {

    return 0;
}
