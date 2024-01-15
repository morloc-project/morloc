#  parseRecord :: JsonObj -> (JsonObj, Sequence)
def parseRecord(jsonObj):
    sequence = jsonObj["GBSeq_sequence"].upper() 
    del jsonObj["GBSeq_sequence"]
    return (jsonObj, sequence)

#  labelRef :: Map Accession Clade -> JsonObj -> (JsonObj, Clade)
def labelRef(cladeMap, jsonObj):
    accession = jsonObj["GBSeq_primary-accession"]
    if accession in cladeMap:
        return (jsonObj, cladeMap[accession])
    else:
        return (jsonObj, "")

# setLeafName :: (JsonObj, Clade) -> Str
def setLeafName(meta):
    (jsonObj, clade) = meta
    return ( clade + "|" +
             jsonObj["GBSeq_primary-accession"] + "|" +
             jsonObj["GBSeq_length"]
           )

##### Here is an example record #####
#  {
#    'GBSeq_locus': 'KU976624',
#    'GBSeq_length': '1778',
#    'GBSeq_strandedness': 'single',
#    'GBSeq_moltype': 'cRNA',
#    'GBSeq_topology': 'linear',
#    'GBSeq_division': 'VRL',
#    'GBSeq_update-date': '30-AUG-2021',
#    'GBSeq_create-date': '18-APR-2016',
#    'GBSeq_definition': 'Influenza A virus (A/swine/Mexico/AVX9/2010(H1N1)) segment 4 hemagglutinin (HA) gene, complete cds',
#    'GBSeq_primary-accession': 'KU976624',
#    'GBSeq_accession-version': 'KU976624.1',
#    'GBSeq_other-seqids': ['gb|KU976624.1|', 'gi|1017035231'],
#    'GBSeq_project': 'PRJNA315383',
#    'GBSeq_source': 'Influenza A virus (A/swine/Mexico/AVX9/2010(H1N1))',
#    'GBSeq_organism': 'Influenza A virus (A/swine/Mexico/AVX9/2010(H1N1))',
#    'GBSeq_taxonomy': 'Viruses; Riboviria; Orthornavirae; Negarnaviricota; Polyploviricotina; Insthoviricetes; Articulavirales; Orthomyxoviridae; Alphainfluenzavirus; Alphainfluenzavirus influenzae',
#    'GBSeq_references': [{'GBReference_reference': '1', 'GBReference_position': '1..1778', 'GBReference_authors': ['Mena,I.', 'Nelson,M.I.', 'Quezada-Monroy,F.', 'Dutta,J.', 'Cortes-Fernandez,R.', 'Lara-Puente,H.J.', 'Castro-Peralta,F.', 'Cunha,L.', 'Trovao,N.', 'Lozano-Dubernard,B.', 'Rambaut,A.', 'van Bakel,H.', 'Garcia-Sastre,A.'], 'GBReference_title': 'Direct Submission', 'GBReference_journal': 'Submitted (23-MAR-2016) Center for Research on Influenza Pathogenesis (CRIP), New York, NY 10029-6574, USA'}],
#    'GBSeq_comment': 'This submission was made by the CEIRS Data Processing and Coordinating Center (DPCC) on behalf of the Center for Research on Influenza Pathogenesis (CRIP). This work was supported by National Institute of Allergy and Infectious Diseases, National Institutes of Health, grant HHSN272201400008C.; GenBank Accession Numbers KU976499, KU976506, KU976538, KU976568, KU976624, KU976693, KU976780, KU976782 represent sequences from the 8 segments of Influenza A virus (A/swine/Mexico/AVX9/2010(H1N1)).; ##Genome-Assembly-Data-START## ; Current Finishing Status :: Finished ; Assembly Method :: Trinity/Inchworm v. v.20140413p1 ; Genome Coverage :: 38628 ; Sequencing Technology :: Illumina HiSeq 2500 ; ##Genome-Assembly-Data-END##',
#    'GBSeq_feature-table':
#      [
#        {
#          'GBFeature_key': 'source',
#          'GBFeature_location': '1..1778',
#          'GBFeature_intervals': [{'GBInterval_from': '1', 'GBInterval_to': '1778', 'GBInterval_accession': 'KU976624.1'}],
#          'GBFeature_quals': [
#            {'GBQualifier_name': 'organism', 'GBQualifier_value': 'Influenza A virus (A/swine/Mexico/AVX9/2010(H1N1))'},
#            {'GBQualifier_name': 'mol_type', 'GBQualifier_value': 'viral cRNA'},
#            {'GBQualifier_name': 'strain', 'GBQualifier_value': 'A/swine/Mexico/AVX9/2010'},
#            {'GBQualifier_name': 'serotype', 'GBQualifier_value': 'H1N1'},
#            {'GBQualifier_name': 'host', 'GBQualifier_value': 'Sus scrofa scrofa'},
#            {'GBQualifier_name': 'db_xref', 'GBQualifier_value': 'taxon:1820594'},
#            {'GBQualifier_name': 'segment', 'GBQualifier_value': '4'},
#            {'GBQualifier_name': 'country', 'GBQualifier_value': 'Mexico'},
#            {'GBQualifier_name': 'collection_date', 'GBQualifier_value': '25-Mar-2010'}
#          ]
#        },
#        {
#          'GBFeature_key': 'gene',
#          'GBFeature_location': '33..1733',
#          'GBFeature_intervals': [{'GBInterval_from': '33', 'GBInterval_to': '1733', 'GBInterval_accession': 'KU976624.1'}],
#          'GBFeature_quals': [
#            {'GBQualifier_name': 'gene', 'GBQualifier_value': 'HA'},
#            {'GBQualifier_name': 'locus_tag', 'GBQualifier_value': 'A1518_AVX_09_HA'}
#          ]
#        },
#        {
#          'GBFeature_key': 'CDS',
#          'GBFeature_location': '33..1733',
#          'GBFeature_intervals': [{'GBInterval_from': '33', 'GBInterval_to': '1733', 'GBInterval_accession': 'KU976624.1'}],
#          'GBFeature_quals': [
#            {'GBQualifier_name': 'gene', 'GBQualifier_value': 'HA'},
#            {'GBQualifier_name': 'locus_tag', 'GBQualifier_value': 'A1518_AVX_09_HA'},
#            {'GBQualifier_name': 'codon_start', 'GBQualifier_value': '1'},
#            {'GBQualifier_name': 'transl_table', 'GBQualifier_value': '1'},
#            {'GBQualifier_name': 'product', 'GBQualifier_value': 'hemagglutinin'},
#            {'GBQualifier_name': 'protein_id', 'GBQualifier_value': 'AMY16254.1'},
#            {'GBQualifier_name': 'translation', 'GBQualifier_value': 'MKAMLVVLLYTFTTANADTLCIGYHANNSTDTVDTVLERNVTVTHSVNLLEDRHNGKLCKLKGVAPLHLGKCNIAGWLLGNPECESLLTTSSWSYIVETSNSDNGTCYPGEFINYEELREQVSSVSSFERFEIFPKASSWPSHETDRGVTAACPHDGASSFYRNLIWLVKKGNSYPKLRKSYINNKGKEVLVLWGIHHPSTSTDQQSLYQHADAYVFVGSSKYSKTFKPEIATRPKVRDQAGRMDYYWTLVEPGDTITFEATGNLVAPRYAFAMKRGSGSGIIISDTPIHDCNTTCQTPKGAINTSLPFQNIHPVTIGECPKYVKSTKLRMATGLRNIPSIQSRGLFGAIAGFIEGGWTGMVDGWYGYHHQNEQGSGYAADRKSTQNAIDGITNKVNSVIEKMNTQFTAVGKEFNHLEIRMENLNKKVDDGFLDVWTYNAELLVLLENERTLDYHDSNVKNLYEKVRSQLKNNAKEIGNGCFEFYHKCDNICMESVKNGTYDYPKYSEEAKLNRKEIDGVKLESTRIYQVLAIYSTAASSLVLLVSLGAISFWMCSNGSLQCRICI'}
#          ]
#        },
#        {
#          'GBFeature_key': 'sig_peptide',
#          'GBFeature_location': '33..83',
#          'GBFeature_intervals': [{'GBInterval_from': '33', 'GBInterval_to': '83', 'GBInterval_accession': 'KU976624.1'}],
#          'GBFeature_quals': [
#            {'GBQualifier_name': 'gene', 'GBQualifier_value': 'HA'},
#            {'GBQualifier_name': 'locus_tag', 'GBQualifier_value': 'A1518_AVX_09_HA'},
#            {'GBQualifier_name': 'peptide', 'GBQualifier_value': 'MKAMLVVLLYTFTTANA'}
#          ]
#        },
#        {
#          'GBFeature_key': 'mat_peptide',
#          'GBFeature_location': '84..1064',
#          'GBFeature_intervals': [{'GBInterval_from': '84', 'GBInterval_to': '1064', 'GBInterval_accession': 'KU976624.1'}],
#          'GBFeature_quals': [
#            {'GBQualifier_name': 'gene', 'GBQualifier_value': 'HA'},
#            {'GBQualifier_name': 'locus_tag', 'GBQualifier_value': 'A1518_AVX_09_HA'},
#            {'GBQualifier_name': 'product', 'GBQualifier_value': 'HA1'},
#            {'GBQualifier_name': 'peptide', 'GBQualifier_value': 'DTLCIGYHANNSTDTVDTVLERNVTVTHSVNLLEDRHNGKLCKLKGVAPLHLGKCNIAGWLLGNPECESLLTTSSWSYIVETSNSDNGTCYPGEFINYEELREQVSSVSSFERFEIFPKASSWPSHETDRGVTAACPHDGASSFYRNLIWLVKKGNSYPKLRKSYINNKGKEVLVLWGIHHPSTSTDQQSLYQHADAYVFVGSSKYSKTFKPEIATRPKVRDQAGRMDYYWTLVEPGDTITFEATGNLVAPRYAFAMKRGSGSGIIISDTPIHDCNTTCQTPKGAINTSLPFQNIHPVTIGECPKYVKSTKLRMATGLRNIPSIQSR'}
#          ]
#        },
#        {
#          'GBFeature_key': 'mat_peptide',
#          'GBFeature_location': '1065..1730',
#          'GBFeature_intervals': [{'GBInterval_from': '1065', 'GBInterval_to': '1730', 'GBInterval_accession': 'KU976624.1'}],
#          'GBFeature_quals': [
#            {'GBQualifier_name': 'gene', 'GBQualifier_value': 'HA'},
#            {'GBQualifier_name': 'locus_tag', 'GBQualifier_value': 'A1518_AVX_09_HA'},
#            {'GBQualifier_name': 'product', 'GBQualifier_value': 'HA2'},
#            {'GBQualifier_name': 'peptide', 'GBQualifier_value': 'GLFGAIAGFIEGGWTGMVDGWYGYHHQNEQGSGYAADRKSTQNAIDGITNKVNSVIEKMNTQFTAVGKEFNHLEIRMENLNKKVDDGFLDVWTYNAELLVLLENERTLDYHDSNVKNLYEKVRSQLKNNAKEIGNGCFEFYHKCDNICMESVKNGTYDYPKYSEEAKLNRKEIDGVKLESTRIYQVLAIYSTAASSLVLLVSLGAISFWMCSNGSLQCRICI'}
#          ]
#        }
#      ],
#    'GBSeq_xrefs': [{'GBXref_dbname': 'BioProject', 'GBXref_id': 'PRJNA315383'}]
#  }
