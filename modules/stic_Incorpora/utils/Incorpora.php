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

/**
 * This class contains all the methods necessarries to stablish the connection with Incorpora WS using SOAPclient PHP plugin.
 */
#[\AllowDynamicProperties]
class Incorpora
{
    private $soapClient;
    private $authParams = array(
        'clave' => '',
        'idUsuario' => '',
        'idioma' => '',
    );
    // private $htmlAuthUsername;
    // private $htmlAuthPassword;
    private $wsdlFileTest;
    private $wsdlFilePro;
    private $errorMsg = array();
    private $logMsg = array();

    public function __construct($module)
    {
        include 'modules/stic_Incorpora/utils/IncorporaCredentials.php';
        $urlPro = $incorporaCredentials['INCORPORA_URL'];
        $urlTest = $incorporaCredentials['INCORPORA_TEST_URL'];
        switch ($module) {
            case 'Accounts':
                // $this->wsdlFileTest = 'modules/stic_Incorpora/utils/Portlet_IncorporaWS_Empresa.xml';
                $this->wsdlFileTest = $urlTest.'/Incorpora/services/Portlet_IncorporaWS_Empresa?wsdl';
                $this->wsdlFilePro = $urlPro.':443/Incorpora/services/Portlet_IncorporaWS_Empresa?wsdl';
                $this->soapFunctionsGet = 'consultaEmpresaV1';
                $this->soapFunctionsNew = 'altaEmpresaV1';
                $this->soapFunctionsEdit = 'modificacionEmpresaV1';
                $this->responseIdField = 'idAltaEmpresa';
                $this->editIdField = 'idEmpresa';

                break;
            case 'Contacts':
                // $this->wsdlFileTest = 'modules/stic_Incorpora/utils/Portlet_IncorporaWS_Beneficiario.xml';
                $this->wsdlFileTest = $urlTest.'/Incorpora/services/Portlet_IncorporaWS_Beneficiario?wsdl';
                $this->wsdlFilePro = $urlPro.':443/Incorpora/services/Portlet_IncorporaWS_Beneficiario?wsdl';
                $this->soapFunctionsGet = 'consultaBeneficiarioV1';
                $this->soapFunctionsNew = 'altaBeneficiarioV1';
                $this->soapFunctionsEdit = 'modificacionBeneficiarioV1';
                $this->responseIdField = 'idAltaBeneficiario';
                $this->editIdField = 'idBeneficiario';
                break;
            case 'stic_Job_Offers';
                // $this->wsdlFileTest = 'modules/stic_Incorpora/utils/Portlet_IncorporaWS_Oferta.xml';
                $this->wsdlFileTest = $urlTest.'/Incorpora/services/Portlet_IncorporaWS_Oferta?wsdl';
                $this->wsdlFilePro = $urlPro.':443/Incorpora/services/Portlet_IncorporaWS_Oferta?wsdl';
                $this->soapFunctionsGet = 'consultaOfertaV1';
                $this->soapFunctionsNew = 'altaOfertaV1';
                $this->soapFunctionsEdit = 'modificacionOfertaV1';
                $this->responseIdField = 'idAltaOferta';
                $this->editIdField = 'idOferta';
                break;
            default;
        }

    }

    /**
     * It stablishes the connection and returns the object that will be used to call the functions. 
     * As Incorpora has IP restriction, the SOAPClient need special parameters/options to stablish the connection, including the HTTP user
     * and password
     *
     * @param array $connectionParams
     * @return void
     */
    public function stablishConnection($connectionParams, $test = false)
    {
        $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . 'Stablishing connection with Incorpora');
        global $mod_strings;
        require_once 'modules/stic_Incorpora/utils/IncorporaCredentials.php';

        $context = stream_context_create([
            'ssl' => [
                'verify_peer' => false,
                'verify_peer_name' => false,
                'allow_self_signed' => true,
            ]
        ]);

        if ($test) {
            $wsdlFile = $this->wsdlFileTest;

            // If there is HTTP authentication, we need these credentials
            // $wsdlFile = $this->wsdlFilePro;
            // $constantHttpUser = 'INCORPORA_TEST_HTTP_USER';
            // $constantHttpPassword = 'INCORPORA_TEST_HTTP_PASSWORD';
            // if (!$httpAuthUsername = $incorporaCredentials[$constantHttpUser]) {
            //     $this->logMsg['err'] = $mod_strings['LBL_INCORPORA_MISSING_CREDENTIALS'] . $constantHttpUser . '<br>';
            // }
            // if (!$httpAuthPassword = $incorporaCredentials[$constantHttpPassword]) {
            //     $this->logMsg['err'] .= $mod_strings['LBL_INCORPORA_MISSING_CREDENTIALS'] . $constantHttpPassword . '<br>';
            // }
            $constantTestUrl = 'INCORPORA_TEST_URL';

            if (!$this->wsdlFileTest) {
                $this->logMsg['err'] .= $mod_strings['LBL_INCORPORA_MISSING_CREDENTIALS'] . $constantTestUrl . '<br>';
            }
            if (isset($this->logMsg['err']) && $this->logMsg['err']) {
                return false;
            }
            $optionsConn = array(
                'uri' => $this->wsdlFileTest,
                'debug' => true,
                'trace' => true,
                'stream_context' => $context,
                'soap_version' => 'SOAP_1_2',
                'cache_wsdl' => WSDL_CACHE_NONE,
                // In PRE we need to set the encoding
                'encoding' => 'UTF-8',
                // If there is HTTP authentication, we need these credentials
                // 'login' => $httpAuthUsername,
                // 'password' => $httpAuthPassword,
            );
        } else {
            $wsdlFile = $this->wsdlFilePro;

            $constantURL = 'INCORPORA_URL';
            if (!$this->wsdlFilePro) {
                $this->logMsg['err'] .= $mod_strings['LBL_RESULTS_INCORPORA_MISSING_SETTINGS'] . $constantURL . '<br>';
            }
            ini_set('default_socket_timeout', 5000); // If this is not set, the soap function sometimes returns the error "Error Fetching http headers "
            $optionsConn = array(
                'uri' => $this->wsdlFilePro,
                'debug' => true,
                'trace' => true,
                'stream_context' => $context,
                'soap_version' => 'SOAP_1_2',
                'cache_wsdl' => WSDL_CACHE_NONE
            );
        }
        $idioma = 'es';
        $this->authParams['clave'] = $connectionParams['password'];
        $this->authParams['idUsuario'] = $connectionParams['username'];
        $this->authParams['idioma'] = $idioma;

        try {
            $this->soapClient = new SoapClient($wsdlFile, $optionsConn);
            $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . 'Stablishing connection with Incorpora   ----> OK');
            return true;
        } catch (Exception $e) {
            $this->logMsg['err'] .= $e->getMessage();
            return false;
        }
    }

    /**
     * It uses the Incorpora fields to create a new record in the Incorpora WS. If the CIF/NIF already exists in the database it will
     * return the ID of the record that exists. If there is any format issue, it will return a log code error.
     *
     * @param array $record
     * @return void
     */
    public function newRecordIncorpora($record)
    {
        try {
            $this->logMsg = array();
            $soapFunction = $this->soapFunctionsNew;
            $sortedRecord = $this->sortSoapParams($this->soapClient->__getFunctions(), $soapFunction, $record);
            $data = array_values($sortedRecord);
            $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . 'Sending New record Incorpora', $sortedRecord);
            $response = $this->soapClient->$soapFunction($this->authParams, ...$data);
            $idField = $this->responseIdField;
            $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . 'Success New record Incorpora', $response);
            $this->logMsg['msg'] = $response->salidaComun->msgRespuesta;
            $this->logMsg['cod'] = $response->salidaComun->codRespuesta;
            $this->logMsg['date'] = $response->salidaComun->fechaRespuesta;
            $this->logMsg['type'] = $response->salidaComun->tipoRespuesta;
            $this->logMsg['new_id'] = empty($response->$idField) ? false : $response->$idField;
            return true;
        } catch (Exception $e) {
            $this->logMsg['err'] = $e->getMessage();
            $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . 'ERROR New record Incorpora', $this->soapClient->__getLastRequest(), $this->logMsg['err']);
            return false;
        }
    }

    /**
     * It uses the Incorpora Id and the Incorpora fields to modify the remote record in Incorpora WS. If it doesn't exist or there is
     * an issue on the format of a field, it will return an error.
     *
     * @param Array $record
     * @param String $recordID
     * @return void
     */
    public function editRecordIncorpora($record, $recordID)
    {
        try {
            $this->logMsg = array();
            $soapFunction = $this->soapFunctionsEdit;
            // From now on, during edit sync, the ID field should be sorted too
            $record[$this->editIdField] = $recordID;
            $sortedRecord = $this->sortSoapParams($this->soapClient->__getFunctions(), $soapFunction, $record);
            $data = array_values($sortedRecord);
            $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . 'Sending New record Incorpora', $sortedRecord);
            $response = $this->soapClient->$soapFunction($this->authParams, ...$data);
            $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . 'Success Edit record Incorpora', $response, $this->soapClient->__getLastRequest());
            $this->logMsg['msg'] = $response->salidaComun->msgRespuesta;
            $this->logMsg['cod'] = $response->salidaComun->codRespuesta;
            $this->logMsg['date'] = $response->salidaComun->fechaRespuesta;
            $this->logMsg['type'] = $response->salidaComun->tipoRespuesta;
            return true;
        } catch (Exception $e) {
            $this->logMsg['err'] = $e->getMessage();
            $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . 'ERROR Edit record Incorpora', $this->soapClient->__getLastRequest(), $this->logMsg['err']);
            return false;
        }
    }


    /**
     * It uses the Incorpora ID to retrieve a record from Incorpora WS. If the ID doesn't exists, it will return an error.
     *
     * @param [type] $moduleName
     * @param [type] $recordId
     * @return void
     */
    public function getRecordIncorpora($recordId)
    {
        try {
            $soapFunction = $this->soapFunctionsGet;
            $response = $this->soapClient->$soapFunction($this->authParams, $recordId);
            $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . 'Success Get record Incorpora', $response);

            $this->logMsg['msg'] = $response->salidaComun->msgRespuesta;
            $this->logMsg['cod'] = $response->salidaComun->codRespuesta;
            $this->logMsg['date'] = $response->salidaComun->fechaRespuesta;
            $this->logMsg['type'] = $response->salidaComun->tipoRespuesta;
            return $response;
        } catch (Exception $e) {
            $this->logMsg['err'] = $e->getMessage();
            $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . 'ERROR Get record Incorpora', $this->soapClient->__getLastRequest(), $this->logMsg['err']);
            return false;
        }
    }

    public function sortSoapParams($wsdlFunctions, $serviceMethod, $params)
    {
        $wsdlFunction = '';
        $requestParams = array();
        // Search for the service method in the wsdl functions
        foreach ($wsdlFunctions as $func) {
            if (stripos($func, "{$serviceMethod}(") !== FALSE) {
                $wsdlFunction = $func;
                break;
            }
        }

        // Now we need to get the order in which the params should be called
        foreach ($params as $k => $v) {
            preg_match_all('/\b' . $k . '\b/', $wsdlFunction, $matchData, PREG_OFFSET_CAPTURE);
            $match = isset($matchData[0][0][1]) ? $matchData[0][0][1] : FALSE;
            if ($match !== FALSE) {
                $requestParams[$k] = $match;
            }
        }

        // Sort the array so that our requestParams are in the correct order
        if (is_array($requestParams)) {
            asort($requestParams);
        } else {
            // Throw an error, the service method or param names was not found.
            $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ', 'The requested service method or parameter names was not found on the web-service. Please check the method name and parameters.');
            $this->logMsg['err'] = 'The requested service method or parameter names was not found on the web-service. Please check the method name and parameters.';
            return false;
        }

        // The $requestParams array now contains the parameter names in the correct order, we just need to add the values now.
        foreach ($requestParams as $k => $paramName) {
            $requestParams[$k] = $params[$k];
        }

        return $requestParams;
    }

    /**
     * It returns the errors array
     * 
     * @return void
     */
    public function getErrorMsg()
    {
        return $this->errorMsg;
    }
    public function getLogMsg()
    {
        return $this->logMsg;
    }
}
