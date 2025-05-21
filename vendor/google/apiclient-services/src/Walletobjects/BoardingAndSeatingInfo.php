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

class BoardingAndSeatingInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $boardingDoor;
  /**
   * @var string
   */
  public $boardingGroup;
  /**
   * @var string
   */
  public $boardingPosition;
  protected $boardingPrivilegeImageType = Image::class;
  protected $boardingPrivilegeImageDataType = '';
  /**
   * @var string
   */
  public $kind;
  protected $seatAssignmentType = LocalizedString::class;
  protected $seatAssignmentDataType = '';
  /**
   * @var string
   */
  public $seatClass;
  /**
   * @var string
   */
  public $seatNumber;
  /**
   * @var string
   */
  public $sequenceNumber;

  /**
   * @param string
   */
  public function setBoardingDoor($boardingDoor)
  {
    $this->boardingDoor = $boardingDoor;
  }
  /**
   * @return string
   */
  public function getBoardingDoor()
  {
    return $this->boardingDoor;
  }
  /**
   * @param string
   */
  public function setBoardingGroup($boardingGroup)
  {
    $this->boardingGroup = $boardingGroup;
  }
  /**
   * @return string
   */
  public function getBoardingGroup()
  {
    return $this->boardingGroup;
  }
  /**
   * @param string
   */
  public function setBoardingPosition($boardingPosition)
  {
    $this->boardingPosition = $boardingPosition;
  }
  /**
   * @return string
   */
  public function getBoardingPosition()
  {
    return $this->boardingPosition;
  }
  /**
   * @param Image
   */
  public function setBoardingPrivilegeImage(Image $boardingPrivilegeImage)
  {
    $this->boardingPrivilegeImage = $boardingPrivilegeImage;
  }
  /**
   * @return Image
   */
  public function getBoardingPrivilegeImage()
  {
    return $this->boardingPrivilegeImage;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
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
  /**
   * @param string
   */
  public function setSeatClass($seatClass)
  {
    $this->seatClass = $seatClass;
  }
  /**
   * @return string
   */
  public function getSeatClass()
  {
    return $this->seatClass;
  }
  /**
   * @param string
   */
  public function setSeatNumber($seatNumber)
  {
    $this->seatNumber = $seatNumber;
  }
  /**
   * @return string
   */
  public function getSeatNumber()
  {
    return $this->seatNumber;
  }
  /**
   * @param string
   */
  public function setSequenceNumber($sequenceNumber)
  {
    $this->sequenceNumber = $sequenceNumber;
  }
  /**
   * @return string
   */
  public function getSequenceNumber()
  {
    return $this->sequenceNumber;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BoardingAndSeatingInfo::class, 'Google_Service_Walletobjects_BoardingAndSeatingInfo');
