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

class GoogleMapsAddressvalidationV1AddressComponent extends \Google\Model
{
  protected $componentNameType = GoogleMapsAddressvalidationV1ComponentName::class;
  protected $componentNameDataType = '';
  /**
   * @var string
   */
  public $componentType;
  /**
   * @var string
   */
  public $confirmationLevel;
  /**
   * @var bool
   */
  public $inferred;
  /**
   * @var bool
   */
  public $replaced;
  /**
   * @var bool
   */
  public $spellCorrected;
  /**
   * @var bool
   */
  public $unexpected;

  /**
   * @param GoogleMapsAddressvalidationV1ComponentName
   */
  public function setComponentName(GoogleMapsAddressvalidationV1ComponentName $componentName)
  {
    $this->componentName = $componentName;
  }
  /**
   * @return GoogleMapsAddressvalidationV1ComponentName
   */
  public function getComponentName()
  {
    return $this->componentName;
  }
  /**
   * @param string
   */
  public function setComponentType($componentType)
  {
    $this->componentType = $componentType;
  }
  /**
   * @return string
   */
  public function getComponentType()
  {
    return $this->componentType;
  }
  /**
   * @param string
   */
  public function setConfirmationLevel($confirmationLevel)
  {
    $this->confirmationLevel = $confirmationLevel;
  }
  /**
   * @return string
   */
  public function getConfirmationLevel()
  {
    return $this->confirmationLevel;
  }
  /**
   * @param bool
   */
  public function setInferred($inferred)
  {
    $this->inferred = $inferred;
  }
  /**
   * @return bool
   */
  public function getInferred()
  {
    return $this->inferred;
  }
  /**
   * @param bool
   */
  public function setReplaced($replaced)
  {
    $this->replaced = $replaced;
  }
  /**
   * @return bool
   */
  public function getReplaced()
  {
    return $this->replaced;
  }
  /**
   * @param bool
   */
  public function setSpellCorrected($spellCorrected)
  {
    $this->spellCorrected = $spellCorrected;
  }
  /**
   * @return bool
   */
  public function getSpellCorrected()
  {
    return $this->spellCorrected;
  }
  /**
   * @param bool
   */
  public function setUnexpected($unexpected)
  {
    $this->unexpected = $unexpected;
  }
  /**
   * @return bool
   */
  public function getUnexpected()
  {
    return $this->unexpected;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1AddressComponent::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1AddressComponent');
