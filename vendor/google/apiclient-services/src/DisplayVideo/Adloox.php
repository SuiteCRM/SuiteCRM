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

namespace Google\Service\DisplayVideo;

class Adloox extends \Google\Collection
{
  protected $collection_key = 'excludedFraudIvtMfaCategories';
  /**
   * @var string
   */
  public $adultExplicitSexualContent;
  /**
   * @var string
   */
  public $armsAmmunitionContent;
  /**
   * @var string
   */
  public $crimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent;
  /**
   * @var string
   */
  public $deathInjuryMilitaryConflictContent;
  /**
   * @var string
   */
  public $debatedSensitiveSocialIssueContent;
  /**
   * @var string
   */
  public $displayIabViewability;
  /**
   * @var string[]
   */
  public $excludedAdlooxCategories;
  /**
   * @var string[]
   */
  public $excludedFraudIvtMfaCategories;
  /**
   * @var string
   */
  public $hateSpeechActsAggressionContent;
  /**
   * @var string
   */
  public $illegalDrugsTobaccoEcigarettesVapingAlcoholContent;
  /**
   * @var string
   */
  public $misinformationContent;
  /**
   * @var string
   */
  public $obscenityProfanityContent;
  /**
   * @var string
   */
  public $onlinePiracyContent;
  /**
   * @var string
   */
  public $spamHarmfulContent;
  /**
   * @var string
   */
  public $terrorismContent;
  /**
   * @var string
   */
  public $videoIabViewability;

  /**
   * @param string
   */
  public function setAdultExplicitSexualContent($adultExplicitSexualContent)
  {
    $this->adultExplicitSexualContent = $adultExplicitSexualContent;
  }
  /**
   * @return string
   */
  public function getAdultExplicitSexualContent()
  {
    return $this->adultExplicitSexualContent;
  }
  /**
   * @param string
   */
  public function setArmsAmmunitionContent($armsAmmunitionContent)
  {
    $this->armsAmmunitionContent = $armsAmmunitionContent;
  }
  /**
   * @return string
   */
  public function getArmsAmmunitionContent()
  {
    return $this->armsAmmunitionContent;
  }
  /**
   * @param string
   */
  public function setCrimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent($crimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent)
  {
    $this->crimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent = $crimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent;
  }
  /**
   * @return string
   */
  public function getCrimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent()
  {
    return $this->crimeHarmfulActsIndividualsSocietyHumanRightsViolationsContent;
  }
  /**
   * @param string
   */
  public function setDeathInjuryMilitaryConflictContent($deathInjuryMilitaryConflictContent)
  {
    $this->deathInjuryMilitaryConflictContent = $deathInjuryMilitaryConflictContent;
  }
  /**
   * @return string
   */
  public function getDeathInjuryMilitaryConflictContent()
  {
    return $this->deathInjuryMilitaryConflictContent;
  }
  /**
   * @param string
   */
  public function setDebatedSensitiveSocialIssueContent($debatedSensitiveSocialIssueContent)
  {
    $this->debatedSensitiveSocialIssueContent = $debatedSensitiveSocialIssueContent;
  }
  /**
   * @return string
   */
  public function getDebatedSensitiveSocialIssueContent()
  {
    return $this->debatedSensitiveSocialIssueContent;
  }
  /**
   * @param string
   */
  public function setDisplayIabViewability($displayIabViewability)
  {
    $this->displayIabViewability = $displayIabViewability;
  }
  /**
   * @return string
   */
  public function getDisplayIabViewability()
  {
    return $this->displayIabViewability;
  }
  /**
   * @param string[]
   */
  public function setExcludedAdlooxCategories($excludedAdlooxCategories)
  {
    $this->excludedAdlooxCategories = $excludedAdlooxCategories;
  }
  /**
   * @return string[]
   */
  public function getExcludedAdlooxCategories()
  {
    return $this->excludedAdlooxCategories;
  }
  /**
   * @param string[]
   */
  public function setExcludedFraudIvtMfaCategories($excludedFraudIvtMfaCategories)
  {
    $this->excludedFraudIvtMfaCategories = $excludedFraudIvtMfaCategories;
  }
  /**
   * @return string[]
   */
  public function getExcludedFraudIvtMfaCategories()
  {
    return $this->excludedFraudIvtMfaCategories;
  }
  /**
   * @param string
   */
  public function setHateSpeechActsAggressionContent($hateSpeechActsAggressionContent)
  {
    $this->hateSpeechActsAggressionContent = $hateSpeechActsAggressionContent;
  }
  /**
   * @return string
   */
  public function getHateSpeechActsAggressionContent()
  {
    return $this->hateSpeechActsAggressionContent;
  }
  /**
   * @param string
   */
  public function setIllegalDrugsTobaccoEcigarettesVapingAlcoholContent($illegalDrugsTobaccoEcigarettesVapingAlcoholContent)
  {
    $this->illegalDrugsTobaccoEcigarettesVapingAlcoholContent = $illegalDrugsTobaccoEcigarettesVapingAlcoholContent;
  }
  /**
   * @return string
   */
  public function getIllegalDrugsTobaccoEcigarettesVapingAlcoholContent()
  {
    return $this->illegalDrugsTobaccoEcigarettesVapingAlcoholContent;
  }
  /**
   * @param string
   */
  public function setMisinformationContent($misinformationContent)
  {
    $this->misinformationContent = $misinformationContent;
  }
  /**
   * @return string
   */
  public function getMisinformationContent()
  {
    return $this->misinformationContent;
  }
  /**
   * @param string
   */
  public function setObscenityProfanityContent($obscenityProfanityContent)
  {
    $this->obscenityProfanityContent = $obscenityProfanityContent;
  }
  /**
   * @return string
   */
  public function getObscenityProfanityContent()
  {
    return $this->obscenityProfanityContent;
  }
  /**
   * @param string
   */
  public function setOnlinePiracyContent($onlinePiracyContent)
  {
    $this->onlinePiracyContent = $onlinePiracyContent;
  }
  /**
   * @return string
   */
  public function getOnlinePiracyContent()
  {
    return $this->onlinePiracyContent;
  }
  /**
   * @param string
   */
  public function setSpamHarmfulContent($spamHarmfulContent)
  {
    $this->spamHarmfulContent = $spamHarmfulContent;
  }
  /**
   * @return string
   */
  public function getSpamHarmfulContent()
  {
    return $this->spamHarmfulContent;
  }
  /**
   * @param string
   */
  public function setTerrorismContent($terrorismContent)
  {
    $this->terrorismContent = $terrorismContent;
  }
  /**
   * @return string
   */
  public function getTerrorismContent()
  {
    return $this->terrorismContent;
  }
  /**
   * @param string
   */
  public function setVideoIabViewability($videoIabViewability)
  {
    $this->videoIabViewability = $videoIabViewability;
  }
  /**
   * @return string
   */
  public function getVideoIabViewability()
  {
    return $this->videoIabViewability;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Adloox::class, 'Google_Service_DisplayVideo_Adloox');
