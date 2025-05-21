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

namespace Google\Service\Walletobjects;

class TicketSeat extends \Google\Model
{
  /**
   * @var string
   */
  public $coach;
  protected $customFareClassType = LocalizedString::class;
  protected $customFareClassDataType = '';
  /**
   * @var string
   */
  public $fareClass;
  /**
   * @var string
   */
  public $seat;
  protected $seatAssignmentType = LocalizedString::class;
  protected $seatAssignmentDataType = '';

  /**
   * @param string
   */
  public function setCoach($coach)
  {
    $this->coach = $coach;
  }
  /**
   * @return string
   */
  public function getCoach()
  {
    return $this->coach;
  }
  /**
   * @param LocalizedString
   */
  public function setCustomFareClass(LocalizedString $customFareClass)
  {
    $this->customFareClass = $customFareClass;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomFareClass()
  {
    return $this->customFareClass;
  }
  /**
   * @param string
   */
  public function setFareClass($fareClass)
  {
    $this->fareClass = $fareClass;
  }
  /**
   * @return string
   */
  public function getFareClass()
  {
    return $this->fareClass;
  }
  /**
   * @param string
   */
  public function setSeat($seat)
  {
    $this->seat = $seat;
  }
  /**
   * @return string
   */
  public function getSeat()
  {
    return $this->seat;
  }
  /**
   * @param LocalizedString
   */
  public function setSeatAssignment(LocalizedString $seatAssignment)
  {
    $this->seatAssignment = $seatAssignment;
  }
  /**
   * @return LocalizedString
   */
  public function getSeatAssignment()
  {
    return $this->seatAssignment;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TicketSeat::class, 'Google_Service_Walletobjects_TicketSeat');
