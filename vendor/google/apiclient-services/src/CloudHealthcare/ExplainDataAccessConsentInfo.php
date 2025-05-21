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

namespace Google\Service\CloudHealthcare;

class ExplainDataAccessConsentInfo extends \Google\Collection
{
  protected $collection_key = 'variants';
  /**
   * @var string[]
   */
  public $cascadeOrigins;
  /**
   * @var string
   */
  public $consentResource;
  /**
   * @var string
   */
  public $enforcementTime;
  protected $matchingAccessorScopesType = ConsentAccessorScope::class;
  protected $matchingAccessorScopesDataType = 'array';
  /**
   * @var string
   */
  public $patientConsentOwner;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string[]
   */
  public $variants;

  /**
   * @param string[]
   */
  public function setCascadeOrigins($cascadeOrigins)
  {
    $this->cascadeOrigins = $cascadeOrigins;
  }
  /**
   * @return string[]
   */
  public function getCascadeOrigins()
  {
    return $this->cascadeOrigins;
  }
  /**
   * @param string
   */
  public function setConsentResource($consentResource)
  {
    $this->consentResource = $consentResource;
  }
  /**
   * @return string
   */
  public function getConsentResource()
  {
    return $this->consentResource;
  }
  /**
   * @param string
   */
  public function setEnforcementTime($enforcementTime)
  {
    $this->enforcementTime = $enforcementTime;
  }
  /**
   * @return string
   */
  public function getEnforcementTime()
  {
    return $this->enforcementTime;
  }
  /**
   * @param ConsentAccessorScope[]
   */
  public function setMatchingAccessorScopes($matchingAccessorScopes)
  {
    $this->matchingAccessorScopes = $matchingAccessorScopes;
  }
  /**
   * @return ConsentAccessorScope[]
   */
  public function getMatchingAccessorScopes()
  {
    return $this->matchingAccessorScopes;
  }
  /**
   * @param string
   */
  public function setPatientConsentOwner($patientConsentOwner)
  {
    $this->patientConsentOwner = $patientConsentOwner;
  }
  /**
   * @return string
   */
  public function getPatientConsentOwner()
  {
    return $this->patientConsentOwner;
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
   * @param string[]
   */
  public function setVariants($variants)
  {
    $this->variants = $variants;
  }
  /**
   * @return string[]
   */
  public function getVariants()
  {
    return $this->variants;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExplainDataAccessConsentInfo::class, 'Google_Service_CloudHealthcare_ExplainDataAccessConsentInfo');
