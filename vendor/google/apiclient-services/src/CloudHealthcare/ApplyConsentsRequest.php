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

class ApplyConsentsRequest extends \Google\Model
{
  protected $patientScopeType = PatientScope::class;
  protected $patientScopeDataType = '';
  protected $timeRangeType = TimeRange::class;
  protected $timeRangeDataType = '';
  /**
   * @var bool
   */
  public $validateOnly;

  /**
   * @param PatientScope
   */
  public function setPatientScope(PatientScope $patientScope)
  {
    $this->patientScope = $patientScope;
  }
  /**
   * @return PatientScope
   */
  public function getPatientScope()
  {
    return $this->patientScope;
  }
  /**
   * @param TimeRange
   */
  public function setTimeRange(TimeRange $timeRange)
  {
    $this->timeRange = $timeRange;
  }
  /**
   * @return TimeRange
   */
  public function getTimeRange()
  {
    return $this->timeRange;
  }
  /**
   * @param bool
   */
  public function setValidateOnly($validateOnly)
  {
    $this->validateOnly = $validateOnly;
  }
  /**
   * @return bool
   */
  public function getValidateOnly()
  {
    return $this->validateOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ApplyConsentsRequest::class, 'Google_Service_CloudHealthcare_ApplyConsentsRequest');
