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

namespace Google\Service\CloudControlsPartnerService;

class AccessApprovalRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $requestTime;
  /**
   * @var string
   */
  public $requestedExpirationTime;
  protected $requestedReasonType = AccessReason::class;
  protected $requestedReasonDataType = '';

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
  public function setRequestTime($requestTime)
  {
    $this->requestTime = $requestTime;
  }
  /**
   * @return string
   */
  public function getRequestTime()
  {
    return $this->requestTime;
  }
  /**
   * @param string
   */
  public function setRequestedExpirationTime($requestedExpirationTime)
  {
    $this->requestedExpirationTime = $requestedExpirationTime;
  }
  /**
   * @return string
   */
  public function getRequestedExpirationTime()
  {
    return $this->requestedExpirationTime;
  }
  /**
   * @param AccessReason
   */
  public function setRequestedReason(AccessReason $requestedReason)
  {
    $this->requestedReason = $requestedReason;
  }
  /**
   * @return AccessReason
   */
  public function getRequestedReason()
  {
    return $this->requestedReason;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AccessApprovalRequest::class, 'Google_Service_CloudControlsPartnerService_AccessApprovalRequest');
