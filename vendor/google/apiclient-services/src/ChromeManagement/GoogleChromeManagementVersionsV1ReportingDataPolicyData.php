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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementVersionsV1ReportingDataPolicyData extends \Google\Collection
{
  protected $collection_key = 'conflicts';
  protected $conflictsType = GoogleChromeManagementVersionsV1ReportingDataConflictingPolicyData::class;
  protected $conflictsDataType = 'array';
  /**
   * @var string
   */
  public $error;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $source;
  /**
   * @var string
   */
  public $value;

  /**
   * @param GoogleChromeManagementVersionsV1ReportingDataConflictingPolicyData[]
   */
  public function setConflicts($conflicts)
  {
    $this->conflicts = $conflicts;
  }
  /**
   * @return GoogleChromeManagementVersionsV1ReportingDataConflictingPolicyData[]
   */
  public function getConflicts()
  {
    return $this->conflicts;
  }
  /**
   * @param string
   */
  public function setError($error)
  {
    $this->error = $error;
  }
  /**
   * @return string
   */
  public function getError()
  {
    return $this->error;
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
  public function setSource($source)
  {
    $this->source = $source;
  }
  /**
   * @return string
   */
  public function getSource()
  {
    return $this->source;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementVersionsV1ReportingDataPolicyData::class, 'Google_Service_ChromeManagement_GoogleChromeManagementVersionsV1ReportingDataPolicyData');
