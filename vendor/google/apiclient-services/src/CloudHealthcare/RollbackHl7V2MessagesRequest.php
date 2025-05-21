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

class RollbackHl7V2MessagesRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $changeType;
  /**
   * @var bool
   */
  public $excludeRollbacks;
  protected $filteringFieldsType = RollbackHL7MessagesFilteringFields::class;
  protected $filteringFieldsDataType = '';
  /**
   * @var bool
   */
  public $force;
  /**
   * @var string
   */
  public $inputGcsObject;
  /**
   * @var string
   */
  public $resultGcsBucket;
  /**
   * @var string
   */
  public $rollbackTime;

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
   * @param bool
   */
  public function setExcludeRollbacks($excludeRollbacks)
  {
    $this->excludeRollbacks = $excludeRollbacks;
  }
  /**
   * @return bool
   */
  public function getExcludeRollbacks()
  {
    return $this->excludeRollbacks;
  }
  /**
   * @param RollbackHL7MessagesFilteringFields
   */
  public function setFilteringFields(RollbackHL7MessagesFilteringFields $filteringFields)
  {
    $this->filteringFields = $filteringFields;
  }
  /**
   * @return RollbackHL7MessagesFilteringFields
   */
  public function getFilteringFields()
  {
    return $this->filteringFields;
  }
  /**
   * @param bool
   */
  public function setForce($force)
  {
    $this->force = $force;
  }
  /**
   * @return bool
   */
  public function getForce()
  {
    return $this->force;
  }
  /**
   * @param string
   */
  public function setInputGcsObject($inputGcsObject)
  {
    $this->inputGcsObject = $inputGcsObject;
  }
  /**
   * @return string
   */
  public function getInputGcsObject()
  {
    return $this->inputGcsObject;
  }
  /**
   * @param string
   */
  public function setResultGcsBucket($resultGcsBucket)
  {
    $this->resultGcsBucket = $resultGcsBucket;
  }
  /**
   * @return string
   */
  public function getResultGcsBucket()
  {
    return $this->resultGcsBucket;
  }
  /**
   * @param string
   */
  public function setRollbackTime($rollbackTime)
  {
    $this->rollbackTime = $rollbackTime;
  }
  /**
   * @return string
   */
  public function getRollbackTime()
  {
    return $this->rollbackTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RollbackHl7V2MessagesRequest::class, 'Google_Service_CloudHealthcare_RollbackHl7V2MessagesRequest');
