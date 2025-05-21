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

class PersonCore extends \Google\Collection
{
  protected $collection_key = 'phoneNumbers';
  /**
   * @var string
   */
  public $addressMeAs;
  protected $adminToType = PersonCore::class;
  protected $adminToDataType = 'array';
  protected $adminsType = PersonCore::class;
  protected $adminsDataType = 'array';
  /**
   * @var string
   */
  public $availabilityStatus;
  protected $birthdayType = Date::class;
  protected $birthdayDataType = '';
  protected $calendarUrlType = SafeUrlProto::class;
  protected $calendarUrlDataType = '';
  protected $chatUrlType = SafeUrlProto::class;
  protected $chatUrlDataType = '';
  /**
   * @var string
   */
  public $costCenter;
  /**
   * @var string
   */
  public $department;
  protected $directReportsType = PersonCore::class;
  protected $directReportsDataType = 'array';
  protected $dottedLineManagersType = PersonCore::class;
  protected $dottedLineManagersDataType = 'array';
  protected $dottedLineReportsType = PersonCore::class;
  protected $dottedLineReportsDataType = 'array';
  /**
   * @var string[]
   */
  public $emails;
  /**
   * @var string
   */
  public $employeeId;
  /**
   * @var string
   */
  public $fingerprint;
  /**
   * @var string
   */
  public $ftePermille;
  protected $geoLocationType = MapInfo::class;
  protected $geoLocationDataType = '';
  /**
   * @var string
   */
  public $gmailUrl;
  /**
   * @var string
   */
  public $jobTitle;
  /**
   * @var string[]
   */
  public $keywordTypes;
  /**
   * @var string[]
   */
  public $keywords;
  protected $linksType = EnterpriseTopazFrontendTeamsLink::class;
  protected $linksDataType = 'array';
  /**
   * @var string
   */
  public $location;
  protected $managersType = PersonCore::class;
  protected $managersDataType = 'array';
  /**
   * @var string
   */
  public $mission;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $officeLocation;
  /**
   * @var string
   */
  public $personId;
  protected $phoneNumbersType = EnterpriseTopazFrontendTeamsPersonCorePhoneNumber::class;
  protected $phoneNumbersDataType = 'array';
  protected $photoUrlType = SafeUrlProto::class;
  protected $photoUrlDataType = '';
  /**
   * @var string
   */
  public $postalAddress;
  /**
   * @var int
   */
  public $totalDirectReportsCount;
  /**
   * @var int
   */
  public $totalDlrCount;
  /**
   * @var string
   */
  public $totalFteCount;
  /**
   * @var string
   */
  public $username;
  /**
   * @var string
   */
  public $waldoComeBackTime;

  /**
   * @param string
   */
  public function setAddressMeAs($addressMeAs)
  {
    $this->addressMeAs = $addressMeAs;
  }
  /**
   * @return string
   */
  public function getAddressMeAs()
  {
    return $this->addressMeAs;
  }
  /**
   * @param PersonCore[]
   */
  public function setAdminTo($adminTo)
  {
    $this->adminTo = $adminTo;
  }
  /**
   * @return PersonCore[]
   */
  public function getAdminTo()
  {
    return $this->adminTo;
  }
  /**
   * @param PersonCore[]
   */
  public function setAdmins($admins)
  {
    $this->admins = $admins;
  }
  /**
   * @return PersonCore[]
   */
  public function getAdmins()
  {
    return $this->admins;
  }
  /**
   * @param string
   */
  public function setAvailabilityStatus($availabilityStatus)
  {
    $this->availabilityStatus = $availabilityStatus;
  }
  /**
   * @return string
   */
  public function getAvailabilityStatus()
  {
    return $this->availabilityStatus;
  }
  /**
   * @param Date
   */
  public function setBirthday(Date $birthday)
  {
    $this->birthday = $birthday;
  }
  /**
   * @return Date
   */
  public function getBirthday()
  {
    return $this->birthday;
  }
  /**
   * @param SafeUrlProto
   */
  public function setCalendarUrl(SafeUrlProto $calendarUrl)
  {
    $this->calendarUrl = $calendarUrl;
  }
  /**
   * @return SafeUrlProto
   */
  public function getCalendarUrl()
  {
    return $this->calendarUrl;
  }
  /**
   * @param SafeUrlProto
   */
  public function setChatUrl(SafeUrlProto $chatUrl)
  {
    $this->chatUrl = $chatUrl;
  }
  /**
   * @return SafeUrlProto
   */
  public function getChatUrl()
  {
    return $this->chatUrl;
  }
  /**
   * @param string
   */
  public function setCostCenter($costCenter)
  {
    $this->costCenter = $costCenter;
  }
  /**
   * @return string
   */
  public function getCostCenter()
  {
    return $this->costCenter;
  }
  /**
   * @param string
   */
  public function setDepartment($department)
  {
    $this->department = $department;
  }
  /**
   * @return string
   */
  public function getDepartment()
  {
    return $this->department;
  }
  /**
   * @param PersonCore[]
   */
  public function setDirectReports($directReports)
  {
    $this->directReports = $directReports;
  }
  /**
   * @return PersonCore[]
   */
  public function getDirectReports()
  {
    return $this->directReports;
  }
  /**
   * @param PersonCore[]
   */
  public function setDottedLineManagers($dottedLineManagers)
  {
    $this->dottedLineManagers = $dottedLineManagers;
  }
  /**
   * @return PersonCore[]
   */
  public function getDottedLineManagers()
  {
    return $this->dottedLineManagers;
  }
  /**
   * @param PersonCore[]
   */
  public function setDottedLineReports($dottedLineReports)
  {
    $this->dottedLineReports = $dottedLineReports;
  }
  /**
   * @return PersonCore[]
   */
  public function getDottedLineReports()
  {
    return $this->dottedLineReports;
  }
  /**
   * @param string[]
   */
  public function setEmails($emails)
  {
    $this->emails = $emails;
  }
  /**
   * @return string[]
   */
  public function getEmails()
  {
    return $this->emails;
  }
  /**
   * @param string
   */
  public function setEmployeeId($employeeId)
  {
    $this->employeeId = $employeeId;
  }
  /**
   * @return string
   */
  public function getEmployeeId()
  {
    return $this->employeeId;
  }
  /**
   * @param string
   */
  public function setFingerprint($fingerprint)
  {
    $this->fingerprint = $fingerprint;
  }
  /**
   * @return string
   */
  public function getFingerprint()
  {
    return $this->fingerprint;
  }
  /**
   * @param string
   */
  public function setFtePermille($ftePermille)
  {
    $this->ftePermille = $ftePermille;
  }
  /**
   * @return string
   */
  public function getFtePermille()
  {
    return $this->ftePermille;
  }
  /**
   * @param MapInfo
   */
  public function setGeoLocation(MapInfo $geoLocation)
  {
    $this->geoLocation = $geoLocation;
  }
  /**
   * @return MapInfo
   */
  public function getGeoLocation()
  {
    return $this->geoLocation;
  }
  /**
   * @param string
   */
  public function setGmailUrl($gmailUrl)
  {
    $this->gmailUrl = $gmailUrl;
  }
  /**
   * @return string
   */
  public function getGmailUrl()
  {
    return $this->gmailUrl;
  }
  /**
   * @param string
   */
  public function setJobTitle($jobTitle)
  {
    $this->jobTitle = $jobTitle;
  }
  /**
   * @return string
   */
  public function getJobTitle()
  {
    return $this->jobTitle;
  }
  /**
   * @param string[]
   */
  public function setKeywordTypes($keywordTypes)
  {
    $this->keywordTypes = $keywordTypes;
  }
  /**
   * @return string[]
   */
  public function getKeywordTypes()
  {
    return $this->keywordTypes;
  }
  /**
   * @param string[]
   */
  public function setKeywords($keywords)
  {
    $this->keywords = $keywords;
  }
  /**
   * @return string[]
   */
  public function getKeywords()
  {
    return $this->keywords;
  }
  /**
   * @param EnterpriseTopazFrontendTeamsLink[]
   */
  public function setLinks($links)
  {
    $this->links = $links;
  }
  /**
   * @return EnterpriseTopazFrontendTeamsLink[]
   */
  public function getLinks()
  {
    return $this->links;
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
   * @param PersonCore[]
   */
  public function setManagers($managers)
  {
    $this->managers = $managers;
  }
  /**
   * @return PersonCore[]
   */
  public function getManagers()
  {
    return $this->managers;
  }
  /**
   * @param string
   */
  public function setMission($mission)
  {
    $this->mission = $mission;
  }
  /**
   * @return string
   */
  public function getMission()
  {
    return $this->mission;
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
  public function setOfficeLocation($officeLocation)
  {
    $this->officeLocation = $officeLocation;
  }
  /**
   * @return string
   */
  public function getOfficeLocation()
  {
    return $this->officeLocation;
  }
  /**
   * @param string
   */
  public function setPersonId($personId)
  {
    $this->personId = $personId;
  }
  /**
   * @return string
   */
  public function getPersonId()
  {
    return $this->personId;
  }
  /**
   * @param EnterpriseTopazFrontendTeamsPersonCorePhoneNumber[]
   */
  public function setPhoneNumbers($phoneNumbers)
  {
    $this->phoneNumbers = $phoneNumbers;
  }
  /**
   * @return EnterpriseTopazFrontendTeamsPersonCorePhoneNumber[]
   */
  public function getPhoneNumbers()
  {
    return $this->phoneNumbers;
  }
  /**
   * @param SafeUrlProto
   */
  public function setPhotoUrl(SafeUrlProto $photoUrl)
  {
    $this->photoUrl = $photoUrl;
  }
  /**
   * @return SafeUrlProto
   */
  public function getPhotoUrl()
  {
    return $this->photoUrl;
  }
  /**
   * @param string
   */
  public function setPostalAddress($postalAddress)
  {
    $this->postalAddress = $postalAddress;
  }
  /**
   * @return string
   */
  public function getPostalAddress()
  {
    return $this->postalAddress;
  }
  /**
   * @param int
   */
  public function setTotalDirectReportsCount($totalDirectReportsCount)
  {
    $this->totalDirectReportsCount = $totalDirectReportsCount;
  }
  /**
   * @return int
   */
  public function getTotalDirectReportsCount()
  {
    return $this->totalDirectReportsCount;
  }
  /**
   * @param int
   */
  public function setTotalDlrCount($totalDlrCount)
  {
    $this->totalDlrCount = $totalDlrCount;
  }
  /**
   * @return int
   */
  public function getTotalDlrCount()
  {
    return $this->totalDlrCount;
  }
  /**
   * @param string
   */
  public function setTotalFteCount($totalFteCount)
  {
    $this->totalFteCount = $totalFteCount;
  }
  /**
   * @return string
   */
  public function getTotalFteCount()
  {
    return $this->totalFteCount;
  }
  /**
   * @param string
   */
  public function setUsername($username)
  {
    $this->username = $username;
  }
  /**
   * @return string
   */
  public function getUsername()
  {
    return $this->username;
  }
  /**
   * @param string
   */
  public function setWaldoComeBackTime($waldoComeBackTime)
  {
    $this->waldoComeBackTime = $waldoComeBackTime;
  }
  /**
   * @return string
   */
  public function getWaldoComeBackTime()
  {
    return $this->waldoComeBackTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PersonCore::class, 'Google_Service_CloudSearch_PersonCore');
