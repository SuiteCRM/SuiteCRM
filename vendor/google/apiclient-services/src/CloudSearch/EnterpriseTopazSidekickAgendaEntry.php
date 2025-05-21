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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickAgendaEntry extends \Google\Collection
{
  protected $collection_key = 'invitee';
  /**
   * @var string
   */
  public $agendaItemUrl;
  /**
   * @var string
   */
  public $chronology;
  protected $creatorType = EnterpriseTopazSidekickPerson::class;
  protected $creatorDataType = '';
  /**
   * @var string
   */
  public $currentUserAttendingStatus;
  /**
   * @var string
   */
  public $description;
  protected $documentType = EnterpriseTopazSidekickCommonDocument::class;
  protected $documentDataType = 'array';
  /**
   * @var string
   */
  public $endDate;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $endTimeMs;
  /**
   * @var string
   */
  public $eventId;
  /**
   * @var bool
   */
  public $guestsCanInviteOthers;
  /**
   * @var bool
   */
  public $guestsCanModify;
  /**
   * @var bool
   */
  public $guestsCanSeeGuests;
  /**
   * @var string
   */
  public $hangoutId;
  /**
   * @var string
   */
  public $hangoutUrl;
  protected $inviteeType = EnterpriseTopazSidekickPerson::class;
  protected $inviteeDataType = 'array';
  /**
   * @var bool
   */
  public $isAllDay;
  /**
   * @var string
   */
  public $lastModificationTimeMs;
  /**
   * @var string
   */
  public $location;
  /**
   * @var bool
   */
  public $notifyToUser;
  /**
   * @var bool
   */
  public $otherAttendeesExcluded;
  /**
   * @var bool
   */
  public $requesterIsOwner;
  /**
   * @var bool
   */
  public $showFullEventDetailsToUse;
  /**
   * @var string
   */
  public $startDate;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $startTimeMs;
  /**
   * @var string
   */
  public $timeZone;
  /**
   * @var string
   */
  public $title;

  /**
   * @param string
   */
  public function setAgendaItemUrl($agendaItemUrl)
  {
    $this->agendaItemUrl = $agendaItemUrl;
  }
  /**
   * @return string
   */
  public function getAgendaItemUrl()
  {
    return $this->agendaItemUrl;
  }
  /**
   * @param string
   */
  public function setChronology($chronology)
  {
    $this->chronology = $chronology;
  }
  /**
   * @return string
   */
  public function getChronology()
  {
    return $this->chronology;
  }
  /**
   * @param EnterpriseTopazSidekickPerson
   */
  public function setCreator(EnterpriseTopazSidekickPerson $creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return EnterpriseTopazSidekickPerson
   */
  public function getCreator()
  {
    return $this->creator;
  }
  /**
   * @param string
   */
  public function setCurrentUserAttendingStatus($currentUserAttendingStatus)
  {
    $this->currentUserAttendingStatus = $currentUserAttendingStatus;
  }
  /**
   * @return string
   */
  public function getCurrentUserAttendingStatus()
  {
    return $this->currentUserAttendingStatus;
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
   * @param EnterpriseTopazSidekickCommonDocument[]
   */
  public function setDocument($document)
  {
    $this->document = $document;
  }
  /**
   * @return EnterpriseTopazSidekickCommonDocument[]
   */
  public function getDocument()
  {
    return $this->document;
  }
  /**
   * @param string
   */
  public function setEndDate($endDate)
  {
    $this->endDate = $endDate;
  }
  /**
   * @return string
   */
  public function getEndDate()
  {
    return $this->endDate;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param string
   */
  public function setEndTimeMs($endTimeMs)
  {
    $this->endTimeMs = $endTimeMs;
  }
  /**
   * @return string
   */
  public function getEndTimeMs()
  {
    return $this->endTimeMs;
  }
  /**
   * @param string
   */
  public function setEventId($eventId)
  {
    $this->eventId = $eventId;
  }
  /**
   * @return string
   */
  public function getEventId()
  {
    return $this->eventId;
  }
  /**
   * @param bool
   */
  public function setGuestsCanInviteOthers($guestsCanInviteOthers)
  {
    $this->guestsCanInviteOthers = $guestsCanInviteOthers;
  }
  /**
   * @return bool
   */
  public function getGuestsCanInviteOthers()
  {
    return $this->guestsCanInviteOthers;
  }
  /**
   * @param bool
   */
  public function setGuestsCanModify($guestsCanModify)
  {
    $this->guestsCanModify = $guestsCanModify;
  }
  /**
   * @return bool
   */
  public function getGuestsCanModify()
  {
    return $this->guestsCanModify;
  }
  /**
   * @param bool
   */
  public function setGuestsCanSeeGuests($guestsCanSeeGuests)
  {
    $this->guestsCanSeeGuests = $guestsCanSeeGuests;
  }
  /**
   * @return bool
   */
  public function getGuestsCanSeeGuests()
  {
    return $this->guestsCanSeeGuests;
  }
  /**
   * @param string
   */
  public function setHangoutId($hangoutId)
  {
    $this->hangoutId = $hangoutId;
  }
  /**
   * @return string
   */
  public function getHangoutId()
  {
    return $this->hangoutId;
  }
  /**
   * @param string
   */
  public function setHangoutUrl($hangoutUrl)
  {
    $this->hangoutUrl = $hangoutUrl;
  }
  /**
   * @return string
   */
  public function getHangoutUrl()
  {
    return $this->hangoutUrl;
  }
  /**
   * @param EnterpriseTopazSidekickPerson[]
   */
  public function setInvitee($invitee)
  {
    $this->invitee = $invitee;
  }
  /**
   * @return EnterpriseTopazSidekickPerson[]
   */
  public function getInvitee()
  {
    return $this->invitee;
  }
  /**
   * @param bool
   */
  public function setIsAllDay($isAllDay)
  {
    $this->isAllDay = $isAllDay;
  }
  /**
   * @return bool
   */
  public function getIsAllDay()
  {
    return $this->isAllDay;
  }
  /**
   * @param string
   */
  public function setLastModificationTimeMs($lastModificationTimeMs)
  {
    $this->lastModificationTimeMs = $lastModificationTimeMs;
  }
  /**
   * @return string
   */
  public function getLastModificationTimeMs()
  {
    return $this->lastModificationTimeMs;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param bool
   */
  public function setNotifyToUser($notifyToUser)
  {
    $this->notifyToUser = $notifyToUser;
  }
  /**
   * @return bool
   */
  public function getNotifyToUser()
  {
    return $this->notifyToUser;
  }
  /**
   * @param bool
   */
  public function setOtherAttendeesExcluded($otherAttendeesExcluded)
  {
    $this->otherAttendeesExcluded = $otherAttendeesExcluded;
  }
  /**
   * @return bool
   */
  public function getOtherAttendeesExcluded()
  {
    return $this->otherAttendeesExcluded;
  }
  /**
   * @param bool
   */
  public function setRequesterIsOwner($requesterIsOwner)
  {
    $this->requesterIsOwner = $requesterIsOwner;
  }
  /**
   * @return bool
   */
  public function getRequesterIsOwner()
  {
    return $this->requesterIsOwner;
  }
  /**
   * @param bool
   */
  public function setShowFullEventDetailsToUse($showFullEventDetailsToUse)
  {
    $this->showFullEventDetailsToUse = $showFullEventDetailsToUse;
  }
  /**
   * @return bool
   */
  public function getShowFullEventDetailsToUse()
  {
    return $this->showFullEventDetailsToUse;
  }
  /**
   * @param string
   */
  public function setStartDate($startDate)
  {
    $this->startDate = $startDate;
  }
  /**
   * @return string
   */
  public function getStartDate()
  {
    return $this->startDate;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param string
   */
  public function setStartTimeMs($startTimeMs)
  {
    $this->startTimeMs = $startTimeMs;
  }
  /**
   * @return string
   */
  public function getStartTimeMs()
  {
    return $this->startTimeMs;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickAgendaEntry::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickAgendaEntry');
