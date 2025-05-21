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

class FutureReservation extends \Google\Model
{
  /**
   * @var string
   */
  public $autoCreatedReservationsDeleteTime;
  protected $autoCreatedReservationsDurationType = Duration::class;
  protected $autoCreatedReservationsDurationDataType = '';
  /**
   * @var bool
   */
  public $autoDeleteAutoCreatedReservations;
  /**
   * @var string
   */
  public $creationTimestamp;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $namePrefix;
  /**
   * @var string
   */
  public $planningStatus;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $selfLinkWithId;
  protected $shareSettingsType = ShareSettings::class;
  protected $shareSettingsDataType = '';
  protected $specificSkuPropertiesType = FutureReservationSpecificSKUProperties::class;
  protected $specificSkuPropertiesDataType = '';
  protected $statusType = FutureReservationStatus::class;
  protected $statusDataType = '';
  protected $timeWindowType = FutureReservationTimeWindow::class;
  protected $timeWindowDataType = '';
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setAutoCreatedReservationsDeleteTime($autoCreatedReservationsDeleteTime)
  {
    $this->autoCreatedReservationsDeleteTime = $autoCreatedReservationsDeleteTime;
  }
  /**
   * @return string
   */
  public function getAutoCreatedReservationsDeleteTime()
  {
    return $this->autoCreatedReservationsDeleteTime;
  }
  /**
   * @param Duration
   */
  public function setAutoCreatedReservationsDuration(Duration $autoCreatedReservationsDuration)
  {
    $this->autoCreatedReservationsDuration = $autoCreatedReservationsDuration;
  }
  /**
   * @return Duration
   */
  public function getAutoCreatedReservationsDuration()
  {
    return $this->autoCreatedReservationsDuration;
  }
  /**
   * @param bool
   */
  public function setAutoDeleteAutoCreatedReservations($autoDeleteAutoCreatedReservations)
  {
    $this->autoDeleteAutoCreatedReservations = $autoDeleteAutoCreatedReservations;
  }
  /**
   * @return bool
   */
  public function getAutoDeleteAutoCreatedReservations()
  {
    return $this->autoDeleteAutoCreatedReservations;
  }
  /**
   * @param string
   */
  public function setCreationTimestamp($creationTimestamp)
  {
    $this->creationTimestamp = $creationTimestamp;
  }
  /**
   * @return string
   */
  public function getCreationTimestamp()
  {
    return $this->creationTimestamp;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
  public function setNamePrefix($namePrefix)
  {
    $this->namePrefix = $namePrefix;
  }
  /**
   * @return string
   */
  public function getNamePrefix()
  {
    return $this->namePrefix;
  }
  /**
   * @param string
   */
  public function setPlanningStatus($planningStatus)
  {
    $this->planningStatus = $planningStatus;
  }
  /**
   * @return string
   */
  public function getPlanningStatus()
  {
    return $this->planningStatus;
  }
  /**
   * @param string
   */
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
  }
  /**
   * @param string
   */
  public function setSelfLinkWithId($selfLinkWithId)
  {
    $this->selfLinkWithId = $selfLinkWithId;
  }
  /**
   * @return string
   */
  public function getSelfLinkWithId()
  {
    return $this->selfLinkWithId;
  }
  /**
   * @param ShareSettings
   */
  public function setShareSettings(ShareSettings $shareSettings)
  {
    $this->shareSettings = $shareSettings;
  }
  /**
   * @return ShareSettings
   */
  public function getShareSettings()
  {
    return $this->shareSettings;
  }
  /**
   * @param FutureReservationSpecificSKUProperties
   */
  public function setSpecificSkuProperties(FutureReservationSpecificSKUProperties $specificSkuProperties)
  {
    $this->specificSkuProperties = $specificSkuProperties;
  }
  /**
   * @return FutureReservationSpecificSKUProperties
   */
  public function getSpecificSkuProperties()
  {
    return $this->specificSkuProperties;
  }
  /**
   * @param FutureReservationStatus
   */
  public function setStatus(FutureReservationStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return FutureReservationStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param FutureReservationTimeWindow
   */
  public function setTimeWindow(FutureReservationTimeWindow $timeWindow)
  {
    $this->timeWindow = $timeWindow;
  }
  /**
   * @return FutureReservationTimeWindow
   */
  public function getTimeWindow()
  {
    return $this->timeWindow;
  }
  /**
   * @param string
   */
  public function setZone($zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return string
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FutureReservation::class, 'Google_Service_Compute_FutureReservation');
