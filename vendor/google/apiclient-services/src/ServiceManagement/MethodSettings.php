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

namespace Google\Service\ServiceManagement;

class MethodSettings extends \Google\Collection
{
  protected $collection_key = 'autoPopulatedFields';
  /**
   * @var string[]
   */
  public $autoPopulatedFields;
  protected $longRunningType = LongRunning::class;
  protected $longRunningDataType = '';
  /**
   * @var string
   */
  public $selector;

  /**
   * @param string[]
   */
  public function setAutoPopulatedFields($autoPopulatedFields)
  {
    $this->autoPopulatedFields = $autoPopulatedFields;
  }
  /**
   * @return string[]
   */
  public function getAutoPopulatedFields()
  {
    return $this->autoPopulatedFields;
  }
  /**
   * @param LongRunning
   */
  public function setLongRunning(LongRunning $longRunning)
  {
    $this->longRunning = $longRunning;
  }
  /**
   * @return LongRunning
   */
  public function getLongRunning()
  {
    return $this->longRunning;
  }
  /**
   * @param string
   */
  public function setSelector($selector)
  {
    $this->selector = $selector;
  }
  /**
   * @return string
   */
  public function getSelector()
  {
    return $this->selector;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MethodSettings::class, 'Google_Service_ServiceManagement_MethodSettings');
