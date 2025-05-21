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

namespace Google\Service\Compute;

class InstanceGroupManagerResizeRequestStatus extends \Google\Model
{
  protected $errorType = InstanceGroupManagerResizeRequestStatusError::class;
  protected $errorDataType = '';
  protected $lastAttemptType = InstanceGroupManagerResizeRequestStatusLastAttempt::class;
  protected $lastAttemptDataType = '';

  /**
   * @param InstanceGroupManagerResizeRequestStatusError
   */
  public function setError(InstanceGroupManagerResizeRequestStatusError $error)
  {
    $this->error = $error;
  }
  /**
   * @return InstanceGroupManagerResizeRequestStatusError
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param InstanceGroupManagerResizeRequestStatusLastAttempt
   */
  public function setLastAttempt(InstanceGroupManagerResizeRequestStatusLastAttempt $lastAttempt)
  {
    $this->lastAttempt = $lastAttempt;
  }
  /**
   * @return InstanceGroupManagerResizeRequestStatusLastAttempt
   */
  public function getLastAttempt()
  {
    return $this->lastAttempt;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InstanceGroupManagerResizeRequestStatus::class, 'Google_Service_Compute_InstanceGroupManagerResizeRequestStatus');
