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

namespace Google\Service\Appengine;

class Runtime extends \Google\Collection
{
  protected $collection_key = 'warnings';
  protected $decommissionedDateType = Date::class;
  protected $decommissionedDateDataType = '';
  protected $deprecationDateType = Date::class;
  protected $deprecationDateDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $endOfSupportDateType = Date::class;
  protected $endOfSupportDateDataType = '';
  /**
   * @var string
   */
  public $environment;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $stage;
  /**
   * @var string[]
   */
  public $supportedOperatingSystems;
  /**
   * @var string[]
   */
  public $warnings;

  /**
   * @param Date
   */
  public function setDecommissionedDate(Date $decommissionedDate)
  {
    $this->decommissionedDate = $decommissionedDate;
  }
  /**
   * @return Date
   */
  public function getDecommissionedDate()
  {
    return $this->decommissionedDate;
  }
  /**
   * @param Date
   */
  public function setDeprecationDate(Date $deprecationDate)
  {
    $this->deprecationDate = $deprecationDate;
  }
  /**
   * @return Date
   */
  public function getDeprecationDate()
  {
    return $this->deprecationDate;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param Date
   */
  public function setEndOfSupportDate(Date $endOfSupportDate)
  {
    $this->endOfSupportDate = $endOfSupportDate;
  }
  /**
   * @return Date
   */
  public function getEndOfSupportDate()
  {
    return $this->endOfSupportDate;
  }
  /**
   * @param string
   */
  public function setEnvironment($environment)
  {
    $this->environment = $environment;
  }
  /**
   * @return string
   */
  public function getEnvironment()
  {
    return $this->environment;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setStage($stage)
  {
    $this->stage = $stage;
  }
  /**
   * @return string
   */
  public function getStage()
  {
    return $this->stage;
  }
  /**
   * @param string[]
   */
  public function setSupportedOperatingSystems($supportedOperatingSystems)
  {
    $this->supportedOperatingSystems = $supportedOperatingSystems;
  }
  /**
   * @return string[]
   */
  public function getSupportedOperatingSystems()
  {
    return $this->supportedOperatingSystems;
  }
  /**
   * @param string[]
   */
  public function setWarnings($warnings)
  {
    $this->warnings = $warnings;
  }
  /**
   * @return string[]
   */
  public function getWarnings()
  {
    return $this->warnings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Runtime::class, 'Google_Service_Appengine_Runtime');
