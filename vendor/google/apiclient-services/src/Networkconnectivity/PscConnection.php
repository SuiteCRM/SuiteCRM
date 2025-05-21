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

namespace Google\Service\Networkconnectivity;

class PscConnection extends \Google\Model
{
  /**
   * @var string
   */
  public $consumerAddress;
  /**
   * @var string
   */
  public $consumerForwardingRule;
  /**
   * @var string
   */
  public $consumerTargetProject;
  protected $errorDataType = '';
  protected $errorInfoType = GoogleRpcErrorInfo::class;
  protected $errorInfoDataType = '';
  /**
   * @var string
   */
  public $errorType;
  /**
   * @var string
   */
  public $gceOperation;
  /**
   * @var string
   */
  public $ipVersion;
  /**
   * @var string
   */
  public $producerInstanceId;
  /**
   * @var string[]
   */
  public $producerInstanceMetadata;
  /**
   * @var string
   */
  public $pscConnectionId;
  /**
   * @var string
   */
  public $selectedSubnetwork;
  /**
   * @var string
   */
  public $serviceClass;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setConsumerAddress($consumerAddress)
  {
    $this->consumerAddress = $consumerAddress;
  }
  /**
   * @return string
   */
  public function getConsumerAddress()
  {
    return $this->consumerAddress;
  }
  /**
   * @param string
   */
  public function setConsumerForwardingRule($consumerForwardingRule)
  {
    $this->consumerForwardingRule = $consumerForwardingRule;
  }
  /**
   * @return string
   */
  public function getConsumerForwardingRule()
  {
    return $this->consumerForwardingRule;
  }
  /**
   * @param string
   */
  public function setConsumerTargetProject($consumerTargetProject)
  {
    $this->consumerTargetProject = $consumerTargetProject;
  }
  /**
   * @return string
   */
  public function getConsumerTargetProject()
  {
    return $this->consumerTargetProject;
  }
  /**
   * @param GoogleRpcStatus
   */
  public function setError(GoogleRpcStatus $error)
  {
    $this->error = $error;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param GoogleRpcErrorInfo
   */
  public function setErrorInfo(GoogleRpcErrorInfo $errorInfo)
  {
    $this->errorInfo = $errorInfo;
  }
  /**
   * @return GoogleRpcErrorInfo
   */
  public function getErrorInfo()
  {
    return $this->errorInfo;
  }
  /**
   * @param string
   */
  public function setErrorType($errorType)
  {
    $this->errorType = $errorType;
  }
  /**
   * @return string
   */
  public function getErrorType()
  {
    return $this->errorType;
  }
  /**
   * @param string
   */
  public function setGceOperation($gceOperation)
  {
    $this->gceOperation = $gceOperation;
  }
  /**
   * @return string
   */
  public function getGceOperation()
  {
    return $this->gceOperation;
  }
  /**
   * @param string
   */
  public function setIpVersion($ipVersion)
  {
    $this->ipVersion = $ipVersion;
  }
  /**
   * @return string
   */
  public function getIpVersion()
  {
    return $this->ipVersion;
  }
  /**
   * @param string
   */
  public function setProducerInstanceId($producerInstanceId)
  {
    $this->producerInstanceId = $producerInstanceId;
  }
  /**
   * @return string
   */
  public function getProducerInstanceId()
  {
    return $this->producerInstanceId;
  }
  /**
   * @param string[]
   */
  public function setProducerInstanceMetadata($producerInstanceMetadata)
  {
    $this->producerInstanceMetadata = $producerInstanceMetadata;
  }
  /**
   * @return string[]
   */
  public function getProducerInstanceMetadata()
  {
    return $this->producerInstanceMetadata;
  }
  /**
   * @param string
   */
  public function setPscConnectionId($pscConnectionId)
  {
    $this->pscConnectionId = $pscConnectionId;
  }
  /**
   * @return string
   */
  public function getPscConnectionId()
  {
    return $this->pscConnectionId;
  }
  /**
   * @param string
   */
  public function setSelectedSubnetwork($selectedSubnetwork)
  {
    $this->selectedSubnetwork = $selectedSubnetwork;
  }
  /**
   * @return string
   */
  public function getSelectedSubnetwork()
  {
    return $this->selectedSubnetwork;
  }
  /**
   * @param string
   */
  public function setServiceClass($serviceClass)
  {
    $this->serviceClass = $serviceClass;
  }
  /**
   * @return string
   */
  public function getServiceClass()
  {
    return $this->serviceClass;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PscConnection::class, 'Google_Service_Networkconnectivity_PscConnection');
