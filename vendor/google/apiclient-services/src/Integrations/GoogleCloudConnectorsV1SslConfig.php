<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Integrations;

class GoogleCloudConnectorsV1SslConfig extends \Google\Collection
{
  protected $collection_key = 'additionalVariables';
  protected $additionalVariablesType = GoogleCloudConnectorsV1ConfigVariable::class;
  protected $additionalVariablesDataType = 'array';
  /**
   * @var string
   */
  public $clientCertType;
  protected $clientCertificateType = GoogleCloudConnectorsV1Secret::class;
  protected $clientCertificateDataType = '';
  protected $clientPrivateKeyType = GoogleCloudConnectorsV1Secret::class;
  protected $clientPrivateKeyDataType = '';
  protected $clientPrivateKeyPassType = GoogleCloudConnectorsV1Secret::class;
  protected $clientPrivateKeyPassDataType = '';
  protected $privateServerCertificateType = GoogleCloudConnectorsV1Secret::class;
  protected $privateServerCertificateDataType = '';
  /**
   * @var string
   */
  public $serverCertType;
  /**
   * @var string
   */
  public $trustModel;
  /**
   * @var string
   */
  public $type;
  /**
   * @var bool
   */
  public $useSsl;

  /**
   * @param GoogleCloudConnectorsV1ConfigVariable[]
   */
  public function setAdditionalVariables($additionalVariables)
  {
    $this->additionalVariables = $additionalVariables;
  }
  /**
   * @return GoogleCloudConnectorsV1ConfigVariable[]
   */
  public function getAdditionalVariables()
  {
    return $this->additionalVariables;
  }
  /**
   * @param string
   */
  public function setClientCertType($clientCertType)
  {
    $this->clientCertType = $clientCertType;
  }
  /**
   * @return string
   */
  public function getClientCertType()
  {
    return $this->clientCertType;
  }
  /**
   * @param GoogleCloudConnectorsV1Secret
   */
  public function setClientCertificate(GoogleCloudConnectorsV1Secret $clientCertificate)
  {
    $this->clientCertificate = $clientCertificate;
  }
  /**
   * @return GoogleCloudConnectorsV1Secret
   */
  public function getClientCertificate()
  {
    return $this->clientCertificate;
  }
  /**
   * @param GoogleCloudConnectorsV1Secret
   */
  public function setClientPrivateKey(GoogleCloudConnectorsV1Secret $clientPrivateKey)
  {
    $this->clientPrivateKey = $clientPrivateKey;
  }
  /**
   * @return GoogleCloudConnectorsV1Secret
   */
  public function getClientPrivateKey()
  {
    return $this->clientPrivateKey;
  }
  /**
   * @param GoogleCloudConnectorsV1Secret
   */
  public function setClientPrivateKeyPass(GoogleCloudConnectorsV1Secret $clientPrivateKeyPass)
  {
    $this->clientPrivateKeyPass = $clientPrivateKeyPass;
  }
  /**
   * @return GoogleCloudConnectorsV1Secret
   */
  public function getClientPrivateKeyPass()
  {
    return $this->clientPrivateKeyPass;
  }
  /**
   * @param GoogleCloudConnectorsV1Secret
   */
  public function setPrivateServerCertificate(GoogleCloudConnectorsV1Secret $privateServerCertificate)
  {
    $this->privateServerCertificate = $privateServerCertificate;
  }
  /**
   * @return GoogleCloudConnectorsV1Secret
   */
  public function getPrivateServerCertificate()
  {
    return $this->privateServerCertificate;
  }
  /**
   * @param string
   */
  public function setServerCertType($serverCertType)
  {
    $this->serverCertType = $serverCertType;
  }
  /**
   * @return string
   */
  public function getServerCertType()
  {
    return $this->serverCertType;
  }
  /**
   * @param string
   */
  public function setTrustModel($trustModel)
  {
    $this->trustModel = $trustModel;
  }
  /**
   * @return string
   */
  public function getTrustModel()
  {
    return $this->trustModel;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param bool
   */
  public function setUseSsl($useSsl)
  {
    $this->useSsl = $useSsl;
  }
  /**
   * @return bool
   */
  public function getUseSsl()
  {
    return $this->useSsl;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudConnectorsV1SslConfig::class, 'Google_Service_Integrations_GoogleCloudConnectorsV1SslConfig');
