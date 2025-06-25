<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

$accountDef = array(
    'stic_identification_number_c' => array(
        0 => 'empresaCIF',
        'required' => true,
    ),
    'name' => array(
        0 => 'empresaNombreComercial',
        // 'required' => true,  We don't duplicate requirement in required field of modules
    ),
    'inc_address_street_type_c' => array(
        0 => 'empresaDireccionTipoVia',
        1 => 'transformedListConsulta',
        2 => 'stic_incorpora_addr_street_type_list', // TODO: Incorpora doesn't return the key values of this list
        'required' => true,
    ),
    'inc_address_street_c' => array(
        0 => 'empresaDireccionNombreVia',
        'required' => true,
    ),
    'inc_address_postal_code_c' => array(
        0 => 'empresaDireccionCP',
        'required' => true,
    ),
    'inc_address_num_a_c' => array(
        0 => 'empresaDireccionNumA',
    ),
    'inc_address_num_b_c' => array(
        0 => 'empresaDireccionNumB',
    ),
    'inc_address_block_c' => array(
        0 => 'empresaDireccionEscalera',
    ),
    'inc_address_floor_c' => array(
        0 => 'empresaDireccionPiso',
    ),
    'inc_address_door_c' => array(
        0 => 'empresaDireccionPuerta',
    ),
    'inc_address_district_c' => array(
        0 => 'empresaDireccionBarrio',
    ),
    'inc_country_c' => array(
        0 => 'empresaDireccionPais',
        'required' => true,
    ),
    'inc_state_code_c' => array(
        0 => 'empresaDireccionProvincia',
    ),
    'inc_municipality_code_c' => array(
        0 => 'empresaDireccionMunicipio',
    ),
    'inc_town_code_c' => array(
        0 => 'empresaDireccionPoblacion',
    ),
    'inc_location_c' => array(
        0 => array(
            0 => 'empresaDireccionProvincia',
            1 => 'empresaDireccionMunicipio',
            2 => 'empresaDireccionPoblacion',
        ),
        1 => 'location',
        2 => array(
            0 => 'inc_state_code_c',
            1 => 'inc_municipality_code_c',
            2 => 'inc_town_code_c',
        ),
        'required' => true,
    ),
    'inc_status_c' => array(
        0 => 'empresaEstado',
        'required' => true,
    ),
    'inc_reference_group_c' => array(
        0 => 'empresaGrupoReferencia',
        1 => 'group',
        'required' => true,
    ),
    'inc_reference_entity_c' => array(
        0 => 'empresaEntidadReferencia',
        1 => 'entity',
        'required' => true,
    ),
    'inc_reference_officer_c' => array(
        0 => 'empresaTecnicoReferencia',
        1 => 'officer',
        'required' => true,
    ),
    'inc_size_c' => array(
        0 => 'empresaModalidad',
        'required' => true,
    ),
    'inc_employees_c' => array(
        0 => 'empresaNumTrabajadores',
    ),
    'inc_activity_sector_c' => array(
        0 => 'empresaSectorActividad',
        'required' => true,
    ),
    'inc_cnae_n1_c' => array(
        0 => 'empresaCNAE_N1',
        'required' => true,
    ),
    'inc_cnae_n2_c' => array(
        0 => 'empresaCNAE_N2',
        1 => 'dropdownDependent',
        2 => 'inc_cnae_n1_c',
        'required' => true,
    ),
    'inc_cnae_n3_c' => array(
        0 => 'empresaCNAE_N3',
        1 => 'dropdownDependent',
        2 => 'inc_cnae_n2_c',
        'required' => true,
    ),
    'inc_occupations_c' => array( // TODO: Solo se puede subir un elemento a Incorpora
        0 => 'empresaOcupaciones',
        1 => 'multienum',
    ),
    'inc_type_c' => array(
        0 => 'empresaTipo',
        'required' => true,
    ),
    'inc_company_territory_scop_c' => array(
        0 => 'empresaAmbitoConvenio',
        'required' => true,
    ),
    'inc_observations_c' => array(
        0 => 'empresaObservaciones',
    ),
    'phone_office' => array(
        0 => 'empresaTelefono',
        'required' => true,
    ),
    'phone_fax' => array(
        0 => 'empresaFax',
    ),
    'email1' => array(
        0 => 'empresaEmail',
        'required' => true,
    ),
    'inc_communications_language_c' => array(
        0 => 'empresaIdiomaContacto',
    ),
    'inc_collection_origin_c' => array(
        0 => 'empresaOrigenCaptacion',
        'required' => true,
    ),
    'inc_contact_date_c' => array(
        0 => 'empresaFechaContacto',
        1 => 'date',
        'required' => true,
    ),
    'inc_contact_telephone_c' => array(
        0 => 'empresaPersonaContactoTelefono',
    ),
    'inc_contact_name_c' => array(
        0 => 'empresaPersonaContactoNombre',
    ),
    'inc_contact_position_c' => array(
        0 => 'empresaPersonaContactoCargo',
    ),
    'inc_agreement_avail_c' => array(
        0 => 'empresaConvenioDisponible',
        1 => 'transformedListNewEdit',
        2 => 'stic_incorpora_yesno_list',
        'required' => true,
    ),
    'inc_agreement_signed_with_c' => array(
        0 => 'empresaConvenioFirmadoCon',
        'agreementRequired' => true,
    ),
    'inc_agreement_start_date_c' => array(
        0 => 'empresaConvenioFechaInicio',
        1 => 'date',
        'agreementRequired' => true,
    ),
    'inc_agreement_comp_person_c' => array(
        0 => 'empresaConvenioEmpFirmanteNombre',
        'agreementRequired' => true,
    ),
    'inc_agreement_comp_position_c' => array(
        0 => 'empresaConvenioEmpFirmanteCargo',
        'agreementRequired' => true,
    ),
    'inc_agreement_inc_person_c' => array(
        0 => 'empresaConvenioIncorporaNombre',
        'agreementRequired' => true,
    ),
    'inc_agreement_inc_position_c' => array(
        0 => 'empresaConvenioIncorporaCargo',
        'agreementRequired' => true,
    ),
    'inc_agreement_territory_scop_c' => array(
        0 => 'empresaConvenioAmbito',
        'agreementRequired' => true,
    ),
    // These fields aren't used anymore in Incorpora, but we need to keep them in order to send a full AltaRequest. We remove the fields from fields_metadata
    'NODATA_inc_agreement_contrib_avail_c' => array(
        0 => 'empresaConvenioAportDisponible',
        1 => 'noData',
    ),
    'NODATA_inc_agreement_contrib_import_c' => array(
        0 => 'empresaConvenioAportImporte',
        1 => 'noData',
    ),
    'NODATA_inc_agreement_contrib_concep_c' => array(
        0 => 'empresaConvenioAportConceptos',
        1 => 'noData',
    ),
    'NODATA_inc_agreement_contrib_date_c' => array(
        0 => 'empresaConvenioAportFecha',
        1 => 'noDataDate',
    ),
);

$contactDef = array(
    'inc_reference_group_c' => array(
        0 => 'beneficiarioGrupoReferencia',
        1 => 'group',
        'required' => true,
    ),
    'inc_reference_entity_c' => array(
        0 => 'beneficiarioEntidadReferencia',
        1 => 'entity',
        'required' => true,
    ),
    'inc_reference_officer_c' => array(
        0 => 'beneficiarioIdTecnicoReferencia',
        1 => 'officer',
        'required' => true,
    ),
    'inc_derivation_c' => array(
        0 => 'beneficiarioDerivacion',
        'required' => true,
    ),
    'stic_identification_type_c' => array(
        0 => 'beneficiarioDocumentoTipo',
        1 => 'transformedList',
        2 => 'stic_contacts_identification_types_list',
        'required' => true,
    ),
    'stic_identification_number_c' => array(
        0 => 'beneficiarioDocumento',
        'required' => true,
    ),
    'first_name' => array(
        0 => 'beneficiarioNombre',
        'required' => true,
    ),
    'last_name' => array(
        0 => 'beneficiarioApellido1',
        1 => 'concat',
        2 => 'beneficiarioApellido2',
    ),
    'none' => array(
        0 => 'beneficiarioApellido2',
    ),
    'inc_address_street_type_c' => array(
        0 => 'beneficiarioDireccionTipoVia',
        1 => 'transformedListConsulta',
        2 => 'stic_incorpora_addr_street_type_list', // TODO: Incorpora doesn't return the key values of this list
        'required' => true,
    ),
    'inc_address_street_c' => array(
        0 => 'beneficiarioDireccionNombreVia',
        'required' => true,
    ),
    'inc_address_postal_code_c' => array(
        0 => 'beneficiarioDireccionCP',
        'required' => true,
    ),
    'inc_address_num_a_c' => array(
        0 => 'beneficiarioDireccionNumA',
    ),
    'inc_address_num_b_c' => array(
        0 => 'beneficiarioDireccionNumB',
    ),
    'inc_address_block_c' => array(
        0 => 'beneficiarioDireccionEscalera',
    ),
    'inc_address_floor_c' => array(
        0 => 'beneficiarioDireccionPiso',
    ),
    'inc_address_door_c' => array(
        0 => 'beneficiarioDireccionPuerta',
    ),
    'inc_address_district_c' => array(
        0 => 'beneficiarioDireccionBarrio',
    ),
    'inc_country_c' => array(
        0 => 'beneficiarioDireccionPais',
        'required' => true,
    ),
    'inc_state_code_c' => array(
        0 => 'beneficiarioDireccionProvincia',
    ),
    'inc_municipality_code_c' => array(
        0 => 'beneficiarioDireccionMunicipio',
    ),
    'inc_town_code_c' => array(
        0 => 'beneficiarioDireccionPoblacion',
    ),
    'inc_location_c' => array(
        0 => array(
            0 => 'beneficiarioDireccionProvincia',
            1 => 'beneficiarioDireccionMunicipio',
            2 => 'beneficiarioDireccionPoblacion',
        ),
        1 => 'location',
        2 => array(
            0 => 'inc_state_code_c',
            1 => 'inc_municipality_code_c',
            2 => 'inc_town_code_c',
        ),
        'required' => true,
    ),
    'phone_home' => array(
        0 => 'beneficiarioTelefonoFijo',
    ),
    'phone_mobile' => array(
        0 => 'beneficiarioTelefonoMovil',
    ),
    'email1' => array(
        0 => 'beneficiarioEMail',
    ),
    'stic_gender_c' => array(
        0 => 'beneficiarioGenero',
        1 => 'transformedList',
        2 => 'stic_genders_list',
        'required' => true,
    ),
    'birthdate' => array(
        0 => 'beneficiarioFechaNacimiento',
        1 => 'date',
        'required' => true,
    ),
    'inc_country_origin_c' => array(
        0 => 'beneficiarioPais',
        'required' => true,
    ),
    'inc_nationality_c' => array(
        0 => 'beneficiarioNacionalidadMultiseleccion',
        1 => 'multienum',
        'required' => true,
        'altaMultienum' => true,
    ),
    'inc_communications_language_c' => array(
        0 => 'beneficiarioIdiomaComunicaciones',
    ),
    'inc_collectives_c' => array(
        0 => 'beneficiarioColectivos',
        1 => 'multienum',
        'required' => true,
    ),
    'inc_incorporation_date_c' => array(
        0 => 'beneficiarioFechaIncorporacion',
        1 => 'date',
        'required' => true,
    ),
    'inc_observations_c' => array(
        0 => 'beneficiarioObservaciones',
    ),
    'inc_children_c' => array(
        0 => 'beneficiarioNumHijos',
    ),
    'inc_disabled_children_c' => array(
        0 => 'beneficiarioNumHijosDiscapacitados',
    ),
    'inc_people_in_charge_c' => array(
        0 => 'beneficiarioNumPersonasACargo',
    ),
    'inc_disability_degree_c' => array(
        0 => 'beneficiarioGradoDiscapacidad',
        1 => 'transformedList',
        2 => 'stic_incorpora_disability_degree_list',
    ),
    'inc_disability_cert_id_c' => array(
        0 => 'beneficiarioCertificadoDiscapacidad',
    ),
    'inc_employment_status_c' => array(
        0 => 'beneficiarioSituacionLaboral',
        1 => 'transformedList',
        2 => 'stic_incorpora_employment_status_list',
        'required' => true,
    ),
    'inc_employ_office_reg_time_c' => array(
        0 => 'beneficiarioTiempoInscrOficTrabajo',
    ),
    'inc_driving_licenses_c' => array(
        0 => 'beneficiarioCarnetsConducir',
    ),
    'inc_requested_employment_c' => array(
        0 => 'beneficiarioOcupacionSolicitada',
        1 => 'multienum',
    ),
    'inc_job_characteristics_c' => array(
        0 => 'beneficiarioPuestoTrabajo',
        1 => 'multienum',
    ),
    'inc_requested_workday_c' => array(
        0 => 'beneficiarioTipoJornada',
        1 => 'dropdown',
    ),
    'inc_geographical_proximity_c' => array(
        0 => 'beneficiarioProximidadGeografica',
        1 => 'dropdown',
    ),
    'inc_travel_availability_c' => array(
        0 => 'beneficiarioDisponibilidadViajar',
        1 => 'transformedList',
        2 => 'stic_incorpora_yesno_list',
        'required' => true,
    ),
    'inc_own_vehicle_c' => array(
        0 => 'beneficiarioVehiculoPropio',
        1 => 'transformedList',
        2 => 'stic_incorpora_yesno_list',
        'required' => true,
    ),
    'inc_car_use_c' => array(
        0 => 'beneficiarioVehiculoUtilizacion',
    ),
    'inc_unwanted_employments_c' => array(
        0 => 'beneficiarioOcupacionesNoQuiere',
    ),
    'inc_lopd_consent_c' => array(
        0 => 'beneficiarioLopdConsentimiento',
        'required' => true,
    ),
    'inc_economic_benefits_c' => array(
        0 => 'beneficiarioPrestacionEconomica',
        1 => 'multienum',
        'altaMultienum' => true,
    ),
    'inc_max_commuting_time_c' => array(
        0 => 'beneficiarioTiempoViajeAsumible',
    ),
    'inc_requested_employment_det_c' => array(
        0 => 'beneficiarioEspecificarOcupaciones',
    ),
);

$offerDef = array(
    'name' => array(
        0 => 'ofertaNombre',
        1 => 'consultaDiferentFields',
        2 => 'ofertaDesc',
    ),
    'inc_reference_group' => array(
        0 => 'ofertaGrupoReferencia',
        1 => 'group',
        'required' => true,
    ),
    'inc_reference_entity' => array(
        0 => 'ofertaEntidadReferencia',
        1 => 'entity',
        'required' => true,
    ),
    'inc_reference_officer' => array(
        0 => 'ofertaIdTecnicoReferencia',
        1 => 'officer',
        'required' => true,
    ),
    'stic_job_offers_accounts' => array(
        0 => 'ofertaEmpresaOfertante',
        1 => 'relation',
        2 => 'inc_id_c',
        5 => 'consultaDiferentFields',
        6 => 'empresaOfertanteId',
    ),
    'inc_cno_n1' => array(
        0 => 'ofertaCnoN1',
        'required' => true,
    ),
    'inc_cno_n2' => array(
        0 => 'ofertaCnoN2',
        1 => 'dropdownDependent',
        2 => 'inc_cno_n1',
        'required' => true,
    ),
    'inc_cno_n3' => array(
        0 => 'ofertaCnoN3',
        1 => 'dropdownDependent',
        2 => 'inc_cno_n2',
        'required' => true,
    ),
    'inc_checkin_date' => array(
        0 => 'fechaRecepcion',
        'required' => true,
    ),
    'offered_positions' => array(
        0 => 'puestosTrabajo',
        1 => 'integer',
        'required' => true,
    ),
    'inc_country' => array(
        0 => 'ofertaPais',
        1 => 'consultaDiferentFields',
        2 => 'ofertaDireccionPais',
        'required' => true,
    ),
    'inc_state_code' => array(
        0 => 'ofertaProvincia',
    ),
    'inc_municipality_code' => array(
        0 => 'ofertaMunicipio',
    ),
    'inc_town_code' => array(
        0 => 'ofertaPoblacion',
    ),
    'inc_location' => array(
        0 => array(
            0 => 'ofertaProvincia',
            1 => 'ofertaMunicipio',
            2 => 'ofertaPoblacion',
        ),
        1 => 'location',
        2 => array(
            0 => 'inc_state_code',
            1 => 'inc_municipality_code',
            2 => 'inc_town_code',
        ),
        5 => 'consultaDiferentFields',
        6 => array(
            0 => 'ofertaDireccionProvincia',
            1 => 'ofertaDireccionMunicipio',
            2 => 'ofertaDireccionPoblacion',
        ),
        'required' => true,
    ),
    'inc_contract_start_date' => array(
        0 => 'fechaIniContrato',
        'required' => true,
    ),
    'inc_register_start_date' => array(
        0 => 'fechaIniInscripcion',
        'required' => true,
    ),
    'inc_register_end_date' => array(
        0 => 'fechaFinInscripcion',
        'required' => true,
    ),
    'inc_contract_type' => array(
        0 => 'tipoContrato',
        'required' => true,
    ),
    'inc_contract_duration' => array(
        0 => 'ofertaDuracionContrato',
        1 => 'transformedList',
        2 => 'stic_incorpora_contract_duration_list',
        'required' => true,
    ),
    'contract_duration_details' => array(
        0 => 'ofertaDuracionContratoDet',
        'required' => true,
    ),
    'inc_working_day' => array(
        0 => 'tipoJornada',
        // 1 => 'transformedList',
        // 2 => 'stic_incorpora_working_day_list',
        'required' => true,
    ),
    'hours_per_week' => array(
        0 => 'ofertaHorasSemanales',
        1 => 'integer',
        'required' => true,
    ),
    'inc_offer_origin' => array(
        0 => 'origenCaptacion',
        'required' => true,
    ),
    'inc_collective_requirements' => array(
        0 => 'colectivo',
        'required' => true,
    ),
    'inc_tasks_responsabilities' => array(
        0 => 'funcionesTareas',
    ),
    'inc_remuneration' => array(
        0 => 'ofertaRemuneracionEuros',
        1 => 'overrideZero',
        'skipAlta' => true,
    ),
    'inc_status' => array(
        0 => 'estado',
        1 => 'transformedListConsulta',
        2 => 'stic_incorpora_job_offers_status_list', // TODO: Incorpora doesn't return the key values of this list
        'skipAlta' => true,
    ),
    'inc_observations' => array(
        0 => 'ofertaObservaciones',
        'skipAlta' => true,
    ),
    'inc_status_details' => array (
        0 => 'estadoDetallado',
        'skipAlta' => true,
    ),
    'inc_professional_licenses' => array (
        0 => 'ofertaCanetConducir',
        'skipAlta' => true,
    ),
    'inc_driving_licenses' => array (
        0 => 'ofertaCanetsProfesionales',
        'skipAlta' => true,
    ),
    'inc_maximum_age' => array (
        0 => 'ofertaEdadMaxima',
        'skipAlta' => true,
    ),
    'inc_minimum_age' => array (
        0 => 'ofertaEdadMinima',
        'skipAlta' => true,
    ),
    'inc_working_experience' => array (
        0 => 'ofertaExperiencia',
        'skipAlta' => true,
    ),
    'inc_education' => array (
        0 => 'ofertaFormacion',
        'skipAlta' => true,
    ),
    'inc_education_languages' => array (
        0 => 'ofertaIdiomas',
        1 => 'multienum',
        'skipAlta' => true,
    ),
    'inc_officer_email' => array (
        0 => 'emailTecnico',
        'skipAlta' => true,
    ),
    'inc_officer_telephone' => array (
        0 => 'telefonoTecnico',
        'skipAlta' => true,
    ),
);
