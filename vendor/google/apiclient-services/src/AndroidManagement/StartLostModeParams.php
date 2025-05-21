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

namespace Google\Service\AndroidManagement;

class StartLostModeParams extends \Google\Model
{
  /**
   * @var string
   */
  public $lostEmailAddress;
  protected $lostMessageType = UserFacingMessage::class;
  protected $lostMessageDataType = '';
  protected $lostOrganizationType = UserFacingMessage::class;
  protected $lostOrganizationDataType = '';
  protected $lostPhoneNumberType = UserFacingMessage::class;
  protected $lostPhoneNumberDataType = '';
  protected $lostStreetAddressType = UserFacingMessage::class;
  protected $lostStreetAddressDataType = '';

  /**
   * @param string
   */
  public function setLostEmailAddress($lostEmailAddress)
  {
    $this->lostEmailAddress = $lostEmailAddress;
  }
  /**
   * @return string
   */
  public function getLostEmailAddress()
  {
    return $this->lostEmailAddress;
  }
  /**
   * @param UserFacingMessage
   */
  public function setLostMessage(UserFacingMessage $lostMessage)
  {
    $this->lostMessage = $lostMessage;
  }
  /**
   * @return UserFacingMessage
   */
  public function getLostMessage()
  {
    return $this->lostMessage;
  }
  /**
   * @param UserFacingMessage
   */
  public function setLostOrganization(UserFacingMessage $lostOrganization)
  {
    $this->lostOrganization = $lostOrganization;
  }
  /**
   * @return UserFacingMessage
   */
  public function getLostOrganization()
  {
    return $this->lostOrganization;
  }
  /**
   * @param UserFacingMessage
   */
  public function setLostPhoneNumber(UserFacingMessage $lostPhoneNumber)
  {
    $this->lostPhoneNumber = $lostPhoneNumber;
  }
  /**
   * @return UserFacingMessage
   */
  public function getLostPhoneNumber()
  {
    return $this->lostPhoneNumber;
  }
  /**
   * @param UserFacingMessage
   */
  public function setLostStreetAddress(UserFacingMessage $lostStreetAddress)
  {
    $this->lostStreetAddress = $lostStreetAddress;
  }
  /**
   * @return UserFacingMessage
   */
  public function getLostStreetAddress()
  {
    return $this->lostStreetAddress;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StartLostModeParams::class, 'Google_Service_AndroidManagement_StartLostModeParams');
