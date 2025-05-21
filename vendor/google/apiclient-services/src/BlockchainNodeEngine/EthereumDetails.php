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

namespace Google\Service\BlockchainNodeEngine;

class EthereumDetails extends \Google\Model
{
  protected $additionalEndpointsType = EthereumEndpoints::class;
  protected $additionalEndpointsDataType = '';
  /**
   * @var bool
   */
  public $apiEnableAdmin;
  /**
   * @var bool
   */
  public $apiEnableDebug;
  /**
   * @var string
   */
  public $consensusClient;
  /**
   * @var string
   */
  public $executionClient;
  protected $gethDetailsType = GethDetails::class;
  protected $gethDetailsDataType = '';
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $nodeType;
  protected $validatorConfigType = ValidatorConfig::class;
  protected $validatorConfigDataType = '';

  /**
   * @param EthereumEndpoints
   */
  public function setAdditionalEndpoints(EthereumEndpoints $additionalEndpoints)
  {
    $this->additionalEndpoints = $additionalEndpoints;
  }
  /**
   * @return EthereumEndpoints
   */
  public function getAdditionalEndpoints()
  {
    return $this->additionalEndpoints;
  }
  /**
   * @param bool
   */
  public function setApiEnableAdmin($apiEnableAdmin)
  {
    $this->apiEnableAdmin = $apiEnableAdmin;
  }
  /**
   * @return bool
   */
  public function getApiEnableAdmin()
  {
    return $this->apiEnableAdmin;
  }
  /**
   * @param bool
   */
  public function setApiEnableDebug($apiEnableDebug)
  {
    $this->apiEnableDebug = $apiEnableDebug;
  }
  /**
   * @return bool
   */
  public function getApiEnableDebug()
  {
    return $this->apiEnableDebug;
  }
  /**
   * @param string
   */
  public function setConsensusClient($consensusClient)
  {
    $this->consensusClient = $consensusClient;
  }
  /**
   * @return string
   */
  public function getConsensusClient()
  {
    return $this->consensusClient;
  }
  /**
   * @param string
   */
  public function setExecutionClient($executionClient)
  {
    $this->executionClient = $executionClient;
  }
  /**
   * @return string
   */
  public function getExecutionClient()
  {
    return $this->executionClient;
  }
  /**
   * @param GethDetails
   */
  public function setGethDetails(GethDetails $gethDetails)
  {
    $this->gethDetails = $gethDetails;
  }
  /**
   * @return GethDetails
   */
  public function getGethDetails()
  {
    return $this->gethDetails;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param string
   */
  public function setNodeType($nodeType)
  {
    $this->nodeType = $nodeType;
  }
  /**
   * @return string
   */
  public function getNodeType()
  {
    return $this->nodeType;
  }
  /**
   * @param ValidatorConfig
   */
  public function setValidatorConfig(ValidatorConfig $validatorConfig)
  {
    $this->validatorConfig = $validatorConfig;
  }
  /**
   * @return ValidatorConfig
   */
  public function getValidatorConfig()
  {
    return $this->validatorConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EthereumDetails::class, 'Google_Service_BlockchainNodeEngine_EthereumDetails');
