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

class ValidatorConfig extends \Google\Collection
{
  protected $collection_key = 'mevRelayUrls';
  /**
   * @var string
   */
  public $beaconFeeRecipient;
  /**
   * @var bool
   */
  public $managedValidatorClient;
  /**
   * @var string[]
   */
  public $mevRelayUrls;

  /**
   * @param string
   */
  public function setBeaconFeeRecipient($beaconFeeRecipient)
  {
    $this->beaconFeeRecipient = $beaconFeeRecipient;
  }
  /**
   * @return string
   */
  public function getBeaconFeeRecipient()
  {
    return $this->beaconFeeRecipient;
  }
  /**
   * @param bool
   */
  public function setManagedValidatorClient($managedValidatorClient)
  {
    $this->managedValidatorClient = $managedValidatorClient;
  }
  /**
   * @return bool
   */
  public function getManagedValidatorClient()
  {
    return $this->managedValidatorClient;
  }
  /**
   * @param string[]
   */
  public function setMevRelayUrls($mevRelayUrls)
  {
    $this->mevRelayUrls = $mevRelayUrls;
  }
  /**
   * @return string[]
   */
  public function getMevRelayUrls()
  {
    return $this->mevRelayUrls;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ValidatorConfig::class, 'Google_Service_BlockchainNodeEngine_ValidatorConfig');
