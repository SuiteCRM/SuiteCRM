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

namespace Google\Service\BigQueryConnectionService;

class ConnectorConfiguration extends \Google\Model
{
  protected $assetType = ConnectorConfigurationAsset::class;
  protected $assetDataType = '';
  protected $authenticationType = ConnectorConfigurationAuthentication::class;
  protected $authenticationDataType = '';
  /**
   * @var string
   */
  public $connectorId;
  protected $endpointType = ConnectorConfigurationEndpoint::class;
  protected $endpointDataType = '';
  protected $networkType = ConnectorConfigurationNetwork::class;
  protected $networkDataType = '';

  /**
   * @param ConnectorConfigurationAsset
   */
  public function setAsset(ConnectorConfigurationAsset $asset)
  {
    $this->asset = $asset;
  }
  /**
   * @return ConnectorConfigurationAsset
   */
  public function getAsset()
  {
    return $this->asset;
  }
  /**
   * @param ConnectorConfigurationAuthentication
   */
  public function setAuthentication(ConnectorConfigurationAuthentication $authentication)
  {
    $this->authentication = $authentication;
  }
  /**
   * @return ConnectorConfigurationAuthentication
   */
  public function getAuthentication()
  {
    return $this->authentication;
  }
  /**
   * @param string
   */
  public function setConnectorId($connectorId)
  {
    $this->connectorId = $connectorId;
  }
  /**
   * @return string
   */
  public function getConnectorId()
  {
    return $this->connectorId;
  }
  /**
   * @param ConnectorConfigurationEndpoint
   */
  public function setEndpoint(ConnectorConfigurationEndpoint $endpoint)
  {
    $this->endpoint = $endpoint;
  }
  /**
   * @return ConnectorConfigurationEndpoint
   */
  public function getEndpoint()
  {
    return $this->endpoint;
  }
  /**
   * @param ConnectorConfigurationNetwork
   */
  public function setNetwork(ConnectorConfigurationNetwork $network)
  {
    $this->network = $network;
  }
  /**
   * @return ConnectorConfigurationNetwork
   */
  public function getNetwork()
  {
    return $this->network;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConnectorConfiguration::class, 'Google_Service_BigQueryConnectionService_ConnectorConfiguration');
