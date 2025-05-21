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

namespace Google\Service\Cloudchannel;

class GoogleCloudChannelV1EntitlementChange extends \Google\Collection
{
  protected $collection_key = 'parameters';
  /**
   * @var string
   */
  public $activationReason;
  /**
   * @var string
   */
  public $cancellationReason;
  /**
   * @var string
   */
  public $changeType;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $entitlement;
  /**
   * @var string
   */
  public $offer;
  /**
   * @var string
   */
  public $operator;
  /**
   * @var string
   */
  public $operatorType;
  /**
   * @var string
   */
  public $otherChangeReason;
  protected $parametersType = GoogleCloudChannelV1Parameter::class;
  protected $parametersDataType = 'array';
  protected $provisionedServiceType = GoogleCloudChannelV1ProvisionedService::class;
  protected $provisionedServiceDataType = '';
  /**
   * @var string
   */
  public $suspensionReason;

  /**
   * @param string
   */
  public function setActivationReason($activationReason)
  {
    $this->activationReason = $activationReason;
  }
  /**
   * @return string
   */
  public function getActivationReason()
  {
    return $this->activationReason;
  }
  /**
   * @param string
   */
  public function setCancellationReason($cancellationReason)
  {
    $this->cancellationReason = $cancellationReason;
  }
  /**
   * @return string
   */
  public function getCancellationReason()
  {
    return $this->cancellationReason;
  }
  /**
   * @param string
   */
  public function setChangeType($changeType)
  {
    $this->changeType = $changeType;
  }
  /**
   * @return string
   */
  public function getChangeType()
  {
    return $this->changeType;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setEntitlement($entitlement)
  {
    $this->entitlement = $entitlement;
  }
  /**
   * @return string
   */
  public function getEntitlement()
  {
    return $this->entitlement;
  }
  /**
   * @param string
   */
  public function setOffer($offer)
  {
    $this->offer = $offer;
  }
  /**
   * @return string
   */
  public function getOffer()
  {
    return $this->offer;
  }
  /**
   * @param string
   */
  public function setOperator($operator)
  {
    $this->operator = $operator;
  }
  /**
   * @return string
   */
  public function getOperator()
  {
    return $this->operator;
  }
  /**
   * @param string
   */
  public function setOperatorType($operatorType)
  {
    $this->operatorType = $operatorType;
  }
  /**
   * @return string
   */
  public function getOperatorType()
  {
    return $this->operatorType;
  }
  /**
   * @param string
   */
  public function setOtherChangeReason($otherChangeReason)
  {
    $this->otherChangeReason = $otherChangeReason;
  }
  /**
   * @return string
   */
  public function getOtherChangeReason()
  {
    return $this->otherChangeReason;
  }
  /**
   * @param GoogleCloudChannelV1Parameter[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return GoogleCloudChannelV1Parameter[]
   */
  public function getParameters()
  {
    return $this->parameters;
  }
  /**
   * @param GoogleCloudChannelV1ProvisionedService
   */
  public function setProvisionedService(GoogleCloudChannelV1ProvisionedService $provisionedService)
  {
    $this->provisionedService = $provisionedService;
  }
  /**
   * @return GoogleCloudChannelV1ProvisionedService
   */
  public function getProvisionedService()
  {
    return $this->provisionedService;
  }
  /**
   * @param string
   */
  public function setSuspensionReason($suspensionReason)
  {
    $this->suspensionReason = $suspensionReason;
  }
  /**
   * @return string
   */
  public function getSuspensionReason()
  {
    return $this->suspensionReason;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudChannelV1EntitlementChange::class, 'Google_Service_Cloudchannel_GoogleCloudChannelV1EntitlementChange');
