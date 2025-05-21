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

namespace Google\Service\AddressValidation;

class GoogleMapsAddressvalidationV1Verdict extends \Google\Model
{
  /**
   * @var bool
   */
  public $addressComplete;
  /**
   * @var string
   */
  public $geocodeGranularity;
  /**
   * @var bool
   */
  public $hasInferredComponents;
  /**
   * @var bool
   */
  public $hasReplacedComponents;
  /**
   * @var bool
   */
  public $hasUnconfirmedComponents;
  /**
   * @var string
   */
  public $inputGranularity;
  /**
   * @var string
   */
  public $validationGranularity;

  /**
   * @param bool
   */
  public function setAddressComplete($addressComplete)
  {
    $this->addressComplete = $addressComplete;
  }
  /**
   * @return bool
   */
  public function getAddressComplete()
  {
    return $this->addressComplete;
  }
  /**
   * @param string
   */
  public function setGeocodeGranularity($geocodeGranularity)
  {
    $this->geocodeGranularity = $geocodeGranularity;
  }
  /**
   * @return string
   */
  public function getGeocodeGranularity()
  {
    return $this->geocodeGranularity;
  }
  /**
   * @param bool
   */
  public function setHasInferredComponents($hasInferredComponents)
  {
    $this->hasInferredComponents = $hasInferredComponents;
  }
  /**
   * @return bool
   */
  public function getHasInferredComponents()
  {
    return $this->hasInferredComponents;
  }
  /**
   * @param bool
   */
  public function setHasReplacedComponents($hasReplacedComponents)
  {
    $this->hasReplacedComponents = $hasReplacedComponents;
  }
  /**
   * @return bool
   */
  public function getHasReplacedComponents()
  {
    return $this->hasReplacedComponents;
  }
  /**
   * @param bool
   */
  public function setHasUnconfirmedComponents($hasUnconfirmedComponents)
  {
    $this->hasUnconfirmedComponents = $hasUnconfirmedComponents;
  }
  /**
   * @return bool
   */
  public function getHasUnconfirmedComponents()
  {
    return $this->hasUnconfirmedComponents;
  }
  /**
   * @param string
   */
  public function setInputGranularity($inputGranularity)
  {
    $this->inputGranularity = $inputGranularity;
  }
  /**
   * @return string
   */
  public function getInputGranularity()
  {
    return $this->inputGranularity;
  }
  /**
   * @param string
   */
  public function setValidationGranularity($validationGranularity)
  {
    $this->validationGranularity = $validationGranularity;
  }
  /**
   * @return string
   */
  public function getValidationGranularity()
  {
    return $this->validationGranularity;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1Verdict::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1Verdict');
