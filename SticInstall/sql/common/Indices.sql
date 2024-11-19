-- Índices identificados para agilizar algunas consultas clave que se ejecutan sobre campos custom y que, por tanto, no pueden definirse vía vardefs.

create index stic_idx_identification_number on contacts_cstm(stic_identification_number_c);
create index stic_idx_identification_number on accounts_cstm(stic_identification_number_c);